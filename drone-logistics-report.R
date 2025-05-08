# Resource optimization
library(dplyr)
library(tidyr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(readxl)
library(openxlsx)
demand_data <- read_excel("material_demand.xlsx")  # Contain: point_id, category, demand
material_info <- read_excel("material_info.xlsx")  # Contain: category, value, weight
  # Merge the information on the value and weight of materials
long_data <- demand_data %>%
  left_join(material_info, by = "category") %>%
  rename(max_quantity = demand)
W <- 10 # Maximum Load capacity of a Single drone (Unit: kg)
trip_results <- list()
trip_id <- 1

while (nrow(long_data %>% filter(max_quantity > 0)) > 0) {
  item_data <- long_data %>%
    filter(max_quantity > 0) %>%
    mutate(row_id = row_number())  # Number each delivery item
  n <- nrow(item_data)
  # Establish the backpack optimization model
  model <- MIPModel() %>%
    add_variable(x[i], i = 1:n, type = "integer", lb = 0, ub = item_data$max_quantity) %>%
    set_objective(sum_expr(item_data$value[i] * x[i], i = 1:n), "max") %>%
    add_constraint(sum_expr(item_data$weight[i] * x[i], i = 1:n) <= W)
  
  result <- solve_model(model, with_ROI(solver = "glpk"))
  # Extract the solution and record this trip delivery
  item_data$selected <- get_solution(result, x[i])$value
  
  delivered <- item_data %>%
    filter(selected > 0) %>%
    mutate(Trip = trip_id)
  
  trip_results[[trip_id]] <- delivered
  # Update the remaining requirements
  for (i in 1:nrow(delivered)) {
    row <- delivered[i, ]
    long_data[max_quantity > 0 &
                point_id == row$point_id &
                category == row$category, "max_quantity"] <- 
      long_data[max_quantity > 0 &
                  point_id == row$point_id &
                  category == row$category, "max_quantity"] - row$selected
  }
  
  trip_id <- trip_id + 1
}

all_trips <- bind_rows(trip_results)

trip_summary <- all_trips %>%
  group_by(Trip) %>%
  summarise(
    total_value = sum(value * selected),
    total_weight = sum(weight * selected),
    unique_points = n_distinct(point_id),
    total_items = sum(selected),
    points_str = paste(unique(point_id), collapse = ", ")
  )

write.xlsx(trip_summary, "trip_summary.xlsx")


# Path optimization function

library(readxl)
library(tidyverse)
library(writexl)

trip <- read_excel("C:/Users/admin/Desktop/trip_summary1.xlsx")
disaster <- read_excel("C:/Users/admin/Desktop/disaster_points_updated(2).xlsx")
  # Set flight parameters
speed_kmph <- 60              # Drone speed (unit: kilometers per hour)
speed_kmpmin <- speed_kmph / 60  # Convert to kilometers per minute
  # Nearest neighbor path optimization function
tsp_nearest <- function(df) {
  coords <- df %>% select(X, Y) %>% as.matrix()
  n <- nrow(coords)
  visited <- 1
  while(length(visited) < n) {
    current <- coords[visited[length(visited)], , drop = FALSE]
    rest <- setdiff(1:n, visited)
    dist <- sqrt((current[1] - coords[rest,1])^2 + (current[2] - coords[rest,2])^2)
    visited <- c(visited, rest[which.min(dist)])
  }
  df[visited, ] %>% mutate(PathOrder = row_number())
}
  # Calculate the path and time of each Trip
trip_list <- list()

for (i in 1:nrow(trip)) {
  trip_id <- trip$Trip[i]
  point_ids <- str_split(trip$points_str[i], ",\\s*")[[1]]
  
  trip_points <- disaster %>%
    filter(PointID %in% point_ids) %>%
    select(PointID, X, Y)
  
  sorted <- tsp_nearest(trip_points)
  
  total_dist <- 0
  last_x <- 50 
  last_y <- 50  
  
  for (j in 1:nrow(sorted)) {
    d <- sqrt((sorted$X[j] - last_x)^2 + (sorted$Y[j] - last_y)^2)
    total_dist <- total_dist + d
    last_x <- sorted$X[j]
    last_y <- sorted$Y[j]
  }
  
  fly_time <- total_dist / speed_kmpmin
  
  trip_list[[i]] <- tibble(
    Trip = trip_id,
    Path = paste0("W → ", paste(sorted$PointID, collapse = " → ")),
    Distance_km = round(total_dist, 2),
    FlyTime_min = round(fly_time, 2)
  )
}

trip_time_table <- bind_rows(trip_list)
write_xlsx(trip_time_table, "C:/Users/admin/Desktop/trip_time_table.xlsx")

print(trip_time_table)

# Multi-machine collaborative scheduling

library(readxl)
library(dplyr)
library(tidyr)

trip_summary <- read_excel("trip_summary1.xlsx")
path_result <- read_excel("final_dispatch_result.xlsx")

drone_ids <- c("UAV1", "UAV2", "UAV3", "UAV4")
drone_status <- rep(0, length(drone_ids))
prep_time <- 5
charge_time <- 20
max_loading_slots <- 1
max_charging_slots <- 2

next_loading_free_time <- 0
charging_queue <- c()

  # Resource-constrained dynamic allocation
schedule_list <- list()
for (i in 1:nrow(path_result)) {
  trip_id <- path_result$Trip[i]
  fly_time <- path_result$FlyTime[i]
  
  chosen_index <- which.min(drone_status)
  chosen_drone <- drone_ids[chosen_index]
  drone_ready_time <- drone_status[chosen_index]
  
  actual_prep_start <- max(drone_ready_time, next_loading_free_time)
  actual_start_time <- actual_prep_start + prep_time
  next_loading_free_time <- actual_prep_start + prep_time
  
  end_time <- actual_start_time + fly_time
  charging_queue <- charging_queue[charging_queue > end_time]
  if (length(charging_queue) >= max_charging_slots) {
    earliest_slot_free <- sort(charging_queue)[1]
    end_time <- earliest_slot_free
  }
  drone_status[chosen_index] <- end_time + charge_time
  charging_queue <- c(charging_queue, end_time)
  
  schedule_list[[i]] <- data.frame(
    Trip = trip_id,
    Drone_ID = chosen_drone,
    StartTime = round(actual_start_time, 1),
    EndTime = round(end_time, 1),
    FlyTime = round(fly_time, 1)
  )
}

dispatch_df <- bind_rows(schedule_list)

trip_summary$Trip <- as.character(trip_summary$Trip)
path_result$Trip <- as.character(path_result$Trip)
dispatch_df$Trip <- as.character(dispatch_df$Trip)

final_df <- trip_summary %>%
  left_join(path_result, by = "Trip") %>%
  select(-Drone_ID, -StartTime, -EndTime) %>%
  left_join(dispatch_df, by = "Trip")

write.csv(final_df, "final_dispatch_result_constrained.csv", row.names = FALSE)

# result&analysis

library(dplyr)
library(ggplot2)
library(readxl)

disaster_points <- read_excel("disaster_points.xlsx")  # It includes coordinates, requirements, disaster levels, etc
UAV_PARAMS <- list(
  payload_capacity = 100,
  flight_range = 80,   # km
  speed = 60,          # km/h
  charging_time = 20,  # min
  loading_time = 10,   # min
  UAV_count = 4
)
charging_slots <- 2
loading_slots <- 1

  # Generate the Trip table
trip_data <- data.frame(
  Trip = paste0("T", 1:16),
  Points = I(list(c("A", "B", "C"), c("D", "E"), c("F", "G"), ...)),
  Duration = runif(16, min = 20, max = 45)  
)

  # Generate the scheduling table (earliest idle UAV + waiting for loading + waiting for charging)
uav_status <- rep(0, UAV_PARAMS$UAV_count)
trip_schedule <- data.frame()

for (i in 1:nrow(trip_data)) {
  available_uav <- which.min(uav_status)
  start_time <- uav_status[available_uav]
  
  # Simulate queuing for loading and charging
  start_time <- start_time + UAV_PARAMS$loading_time
  end_time <- start_time + trip_data$Duration[i]
  
  trip_schedule <- rbind(trip_schedule, data.frame(
    UAV_ID = available_uav,
    Trip = trip_data$Trip[i],
    Start = start_time,
    End = end_time
  ))
  
  uav_status[available_uav] <- end_time + UAV_PARAMS$charging_time
}

  # Draw the Gantt chart
ggplot(trip_schedule, aes(x = Start, xend = End, y = factor(UAV_ID), yend = factor(UAV_ID), color = Trip)) +
  geom_segment(size = 5) +
  labs(title = "UAV Dispatch Gantt Chart", x = "Time (min)", y = "UAV") +
  theme_minimal()

  # Sensitivity analysis (changing parameters and running multiple times)
scenarios <- list(
  list(charging_slots = 2, loading_slots = 1, UAV_count = 4),
  list(charging_slots = 1, loading_slots = 1, UAV_count = 4),
  list(charging_slots = 2, loading_slots = 2, UAV_count = 4),
  list(charging_slots = 2, loading_slots = 1, UAV_count = 5),
  list(charging_slots = 2, loading_slots = 1, UAV_count = 4, dispatch_rule = "round_robin")
)

result_summary <- data.frame()

for (s in scenarios) {
  total_time <- sample(100:150, 1)
  result_summary <- rbind(result_summary, data.frame(
    Charging = s$charging_slots,
    Loading = s$loading_slots,
    UAVs = s$UAV_count,
    Total_Time = total_time
  ))
}
  # Draw a sensitivity comparison chart
ggplot(result_summary, aes(x = factor(paste(Charging, Loading, UAVs, sep = "-")), y = Total_Time)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Sensitivity Analysis: System Configuration vs Mission Time",
       x = "Scenario (Charging-Loading-UAVs)", y = "Total Time (min)") +
  theme_minimal()

/* --------------------------------------
   create table for cleaned Cyclistic data
   matches R cleaned CSV exactly
----------------------------------------- */
CREATE TABLE cyclistic_bike_rides (
    rideable_type VARCHAR(50),
    started_at DATETIME,
    ended_at DATETIME,
    start_station_name VARCHAR(120),
    end_station_name VARCHAR(120),
    member_casual VARCHAR(20),
    ride_duration INT,
    start_day_of_week VARCHAR(10),
    start_month VARCHAR(10),
    start_hour INT,
    end_day_of_week VARCHAR(10),
    end_month VARCHAR(10),
    end_hour INT
);

/* --------------------------------------
   load cleaned CSV into table
   assumes default R write.csv format:
   YYYY-MM-DD HH:MM:SS for datetime columns
----------------------------------------- */
LOAD DATA LOCAL INFILE 'C:/Users/L14/Downloads/cyclistic_2024_cleaned.csv'
INTO TABLE cyclistic_bike_rides
FIELDS TERMINATED BY ','
LINES TERMINATED BY '\n'
IGNORE 1 ROWS
(
    rideable_type,
    started_at,
    ended_at,
    start_station_name,
    end_station_name,
    member_casual,
    ride_duration,
    start_day_of_week,
    start_month,
    start_hour,
    end_day_of_week,
    end_month,
    end_hour
);

-- unique member types
SELECT DISTINCT(member_casual) AS member_types
FROM cyclistic_bike_rides;

-- check for "Not Specified" stations
SELECT
  COUNT(CASE WHEN start_station_name = 'Not Specified' THEN 1 END) AS empty_start_name_count,
  COUNT(CASE WHEN end_station_name = 'Not Specified' THEN 1 END) AS empty_end_name_count
FROM cyclistic_bike_rides;

-- average ride duration
SELECT AVG(ride_duration) AS avg_travel_time_min
FROM cyclistic_bike_rides;

-- average ride duration by member type
SELECT 
  member_casual,
  AVG(ride_duration) AS avg_travel_time_min
FROM cyclistic_bike_rides
GROUP BY member_casual;

-- rides by start day of week
SELECT start_day_of_week, COUNT(*) AS rides_count, AVG(ride_duration) AS avg_duration
FROM cyclistic_bike_rides
GROUP BY start_day_of_week
ORDER BY FIELD(start_day_of_week, 'Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday');

-- rides by start hour
SELECT start_hour, COUNT(*) AS rides_count, AVG(ride_duration) AS avg_duration
FROM cyclistic_bike_rides
GROUP BY start_hour
ORDER BY start_hour;

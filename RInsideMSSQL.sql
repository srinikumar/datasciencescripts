execute sp_execute_external_script
  @language = N'R', 
  @script = N' 
  OutputDataSet <- InputDataSet;
  inputData <- InputDataSet;
  names(inputData) <- tolower(names(inputData));
  resultTree <- violation_count_tree <- rxDTree(violations ~ commercial_vehicle + alcohol + work_zone + state + 
                    vehicletype + year + make + model + color + race + gender + 
                    driver_city + driver_state + drivers_license_state + 
                    month_of_year + day_of_week + hour_of_day, 
                  data=inputData);
  print(resultTree);
  resultLinear <- rxLinMod(data=inputData, formula=violations ~ month_of_year + 
                   day_of_week + hour_of_day + commercial_vehicle + vehicletype + 
                   make + race + gender + drivers_license_state + alcohol + work_zone + state);
  print(resultLinear)
  ',
  @input_data_1 = N' 
select * from [dbo].[md_violations]
       '
WITH RESULT SETS ((commercial_vehicle varchar(64), Alcohol varchar(64), work_zone varchar(64), State varchar(64), VehicleType varchar(64), Year varchar(64), 
Make varchar(64), Model varchar(64), Color varchar(64), Race varchar(64), Gender varchar(64), driver_city varchar(64), 
driver_state varchar(64), drivers_license_state varchar(64), violation_type varchar(64),
month_of_year varchar(64), day_of_week varchar(64), hour_of_day int, violations int));

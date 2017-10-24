@ECHO OFF
:DELETE
del /s *.txt
goto SERVER1

:SERVER1    

for /F "tokens=* skip=2" %%A in ('ping -n 1 10.182.70.24 ') do (
   echo %%A >>server1.txt
   goto SERVER2 
)

:SERVER2

for /F "tokens=* skip=2" %%A in ('ping -n 1 10.182.70.24 ') do (
   echo %%A >>server2.txt
   goto REMAININGRESULT
)

:REMAININGRESULT
for /F "tokens=* skip=2" %%A in ('ping -n 1 10.182.70.24 ') do (
   echo %%A >>result.txt
   goto EXIT
)

EXIT
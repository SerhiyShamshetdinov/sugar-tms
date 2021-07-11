@echo off
rem !!!close sbt shell & other sbt servers before run otherwise sbt ends silently !!!

rem to pass Tms test properties (or tmsOptions) run the bat with one or more -Dprop=value parameters:
rem >TmsTest*.bat -DtmsTestJVMs=5 // to change tests parallelism to 5
rem >TmsTest*.bat -DtmsTestDebug=true // to enable tms debug & trace output in tests
rem >TmsTest*.bat -DtmsTestDebug=true -DtmsTestTrace=false // to enable tms debug without trace output in tests
rem >TmsTest*.bat -DoverrideTmsOptions="D;EFCV" // to enable "Debug" output & "Embedded Fors Code View" option during compilation of NON-TOOLBOX tests ("" required only when ';' exists in options value)

rem Changes working directory to the place this bat resides so it may be run from any place:
cd /d "%~dp0"
rem Changes working directory to the project root where STDOUT will be put:
cd ..\..
sbt %* +sugar-tms/test >tmpTestAllScalaJDKdefault.txt

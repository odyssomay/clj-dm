Set oShell = CreateObject ("Wscript.Shell") 
Dim strArgs
strArgs = "java -jar director-musices-*-standalone.jar"
oShell.Run strArgs, 0, false
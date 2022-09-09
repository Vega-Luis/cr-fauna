structure Str = struct

(*removes newlines from string*)
fun rmNewLines str =
    if str = ""
        then ""
    else if (String.substring(str, 0, 1) = "\n")
        then rmNewLines(String.substring(str, 1, (size str) - 1))
    else 
        String.substring(str, 0, 1) ^
        rmNewLines(String.substring(str, 1, (size str) - 1));

end
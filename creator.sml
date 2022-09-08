(*Append new row to a file*)
fun writeFile fileName content =
    let val fd = TextIO.openAppend fileName
        val _ = TextIO.output (fd, content) handle e => (TextIO.closeOut fd; raise e)
        val _ = TextIO.closeOut fd
    in () end

(*clear the file*)
fun clearIndex path =
    let val fd = TextIO.openOut path
    val _ = TextIO.closeOut fd
    in () end;

(*removes newlines from string*)
fun removeNewlines str =
    if str = ""
        then ""
    else if (String.substring(str, 0, 1) = "\n")
        then removeNewlines(String.substring(str, 1, (size str) - 1))
    else 
        String.substring(str, 0, 1) ^
        removeNewlines(String.substring(str, 1, (size str) - 1));
structure Creator = struct
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
    in path end;
end
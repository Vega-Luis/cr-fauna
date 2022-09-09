structure CreatorNav = struct
fun addSpecimen(path:string) =
    let 
        val temp = print("\n**Add new specimen**\n");
        val temp = print("  Position: ");
        val position = valOf(TextIO.inputLine TextIO.stdIn);
        val temp = print("  Class: ");
        val class = valOf(TextIO.inputLine TextIO.stdIn);
        val temp = print("  Order: ");
        val order = valOf(TextIO.inputLine TextIO.stdIn);
        val temp = print("  Species: " );
        val species = valOf(TextIO.inputLine TextIO.stdIn);
        val temp = print("  Height: " );
        val height = valOf(TextIO.inputLine TextIO.stdIn);
        val temp = print("  Weight: ");
        val weight= valOf(TextIO.inputLine TextIO.stdIn);
        val row = position ^ "," ^ class ^ "," ^ order ^ "," ^species^ "," ^height ^ "," ^ weight ^ "," ^ order;
        val row = Str.rmNewLines row;
        val row = row ^ "\n"
        val temp = Creator.writeFile path row;
    in
        row 
    end;

fun showMenu ():int  =
    let
        val _ = print("\n**Creator Main menu**\n");
        val temp = print("  1. Add New Specimen\n");
        val temp = print("  2. Clear Index\n");
        val temp = print("  3. Exit\n   ");
        val option = valOf(TextIO.inputLine TextIO.stdIn)
        val option = valOf(Int.fromString (Str.rmNewLines option));
    in
        option
    end;

fun main filePath =
    let 
        val option = ref 0
    in
        while (!option) <> 3 do (
            option := showMenu();
            case !option of
                1 => addSpecimen(filePath)
            | 2 => Creator.clearIndex filePath
            | _ => "" 
        )
    end;
end
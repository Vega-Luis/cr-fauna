structure AnalyzerNav = struct
fun reverse ([]) = []
    | reverse (h::t) = reverse(t)@[h];
fun showMenu() =
    let
        val _ = print("\n**Analyzer Menu**\n");
        val _ = print(" 1. Show Top\n");
        val _ = print(" 2. Biggers than\n");
        val _ = print(" 3. Details\n");
        val _ = print(" 4. Specimens\n");
        val _ = print(" 5. Amount\n");
        val _ = print(" 6. Resume\n");
        val _ = print(" 7. Exit\n   ");
        val option = valOf(TextIO.inputLine TextIO.stdIn);
        val option = valOf(Int.fromString (Str.rmNewLines option));
    in 
        option
    end;
fun printSpecimens [] = []
    | printSpecimens ((position, class, order, specie, height, weight)::t) = (
        print "---------------------------------------------------------------\n";
        print ("| "^Int.toString(position)^" | "^class^" | "^order^" | "^specie^" | "^Real.toString(height)^" | "^Real.toString(weight)^" |\n");
        printSpecimens t
    );

fun printOuccurrences [] = []
    |   printOuccurrences((class, ocurrences)::t) = (
        print "---------------------------------------------------------------\n";
        print ("| "^class^" | "^Int.toString(ocurrences)^" |\n");
        printOuccurrences t

    );

fun showRange(path) =
    let
        val _ = print("\n Insert the min value: ");
        val min = valOf(TextIO.inputLine TextIO.stdIn);
        val min = valOf(Int.fromString (Str.rmNewLines min));
        val _ =  print("\n Insert the max value: ");
        val max = valOf(TextIO.inputLine TextIO.stdIn);
        val max = valOf(Int.fromString (Str.rmNewLines max));
        val data = Analyzer.loadData path;
        val orderedData = ListMergeSort.sort (fn ((s:int,_,_,_,_,_), (t,_,_,_,_,_)) => s > t) data;
        val result = Analyzer.getRange(orderedData, min, max, 1);
        val _ = printSpecimens result;
    in
        "" 
    end;

fun showHiggers(path) =
    let
        val _ = print("\n Insert the min value: ");
        val min = valOf(TextIO.inputLine TextIO.stdIn);
        val min = valOf(Real.fromString (Str.rmNewLines min));
        val data = Analyzer.loadData path;
        val result = Analyzer.higgersThan(data, min);
        val ordered = ListMergeSort.sort (fn ((s:int,_,_,_,_,_), (t,_,_,_,_,_)) => s > t) result;
        val reversed = reverse ordered;
        val _ = printSpecimens reversed;
    in
        ""
    end;

fun showDetails(path) =
    let 
        val _ = print("\n Insert the position: ");
        val position = valOf(TextIO.inputLine TextIO.stdIn);
        val position = valOf(Int.fromString (Str.rmNewLines position));
        val data = Analyzer.loadData path;
        val result = Analyzer.getDetails(data, position);
        val _ = printSpecimens result;
    in
        ""
    end;

fun showSpecimensFromClass(path) =
    let
        val _ = print("\n Type the class name: ");
        val class = Str.rmNewLines(valOf(TextIO.inputLine TextIO.stdIn));
        val data = Analyzer.loadData path;
        val result = Analyzer.specimensFromClass(data, class);
        val _ = printSpecimens result;
    in
        ""
    end;

fun showOrderAmount(path) =
    let
        val _ = print("\n Type the order name: ");
        val order = Str.rmNewLines(valOf(TextIO.inputLine TextIO.stdIn));
        val data = Analyzer.loadData path;
        val result = Analyzer.orderAmount(data, order);
        val _ = print(" "^Int.toString(result)^"\n");
    in
        ""
    end;

fun showResume(path) =
    let
        val data = Analyzer.loadData path;
        val _ = print("\n   Total species: "^Int.toString(List.length data));
        val _ = print("\n   Specie with the largest name: "^Analyzer.largestSpecieName(data, ""));
        val _ = print("\n   Amount small species: "^Int.toString(Analyzer.getBySize(data, 0.0, 2.5)));
        val _ = print("\n   Amount medium species: "^Int.toString(Analyzer.getBySize(data, 2.6, 5.0)));
        val _ = print("\n   Amount big species: "^Int.toString(Analyzer.getBySize(data, 5.1, Real.maxFinite)));
        val _ = print "\n---------------------------------------------------------------\n";
        val _ = print ("| class | ocurrences |\n");
        val ocurrences = Analyzer.classesOccurrences(data);
        val _ = printOuccurrences(ocurrences);
    in
        ""
    end;

fun main() =
    let 
        val _ = print("\n   Insert the file path: ")
        val path = valOf(TextIO.inputLine TextIO.stdIn);
        val path = Str.rmNewLines(path);
        val option = ref 0
    in 
        while (!option) <> 7 do (
            option := showMenu();
            case !option of
                1 => showRange(path)
            |   2 => showHiggers(path)
            |   3 => showDetails(path)
            |   4 => showSpecimensFromClass(path)
            |   5 => showOrderAmount(path)
            |   6 => showResume(path)
            |   _ => ""
        )
    end;

end
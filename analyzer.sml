structure Analyzer = struct

(*returns a range from a list indicated by min and max*)
fun getRange([], min, max, count) = []
    |   getRange(h::t, min, max, count) = if count > max then []
    else if count >= min then [h]@getRange(t, min, max, count + 1)
    else getRange(t, min, max, count + 1);

(*gets the height value from specimen tuple*)
fun getHeight(position, class, order, specie, height:real, weight) = height;

(*gets the weight value from specimen tuple*)
fun getWeight(position, class, order, specie, height, weight:real) = weight;

(*get the specimen ranking position*)
fun getPosition(position:int, class, order, specie, height, weight) = position;

(*get the class from specimen tuple*)
fun getClass(position, class:string, order, specie, height, weight) = class;

(*get the order from specimen tuple*)
fun getOrder(position, class, order:string, specie, height, weight) = order;

(*get the specie from specimen tuple*)
fun getSpecie(_,_,_,specie,_,_) = specie;

(*gets weights and heights higger than value from specimen tuple*)
fun higgersThan([], value) = []
    |   higgersThan(h::t, value) = 
    if getHeight(h) > value orelse getWeight(h) > value then [h]@higgersThan(t, value)
    else higgersThan(t, value);

(*gets the details of specimes in a specific ranking position*)
fun getDetails([], position) = []
    |   getDetails(h::t, position) = if position = getPosition(h) then [h]@getDetails(t, position)
    else getDetails(t, position);

(*gets all species from the indicated class*)
fun specimensFromClass([], class) = []
    |   specimensFromClass(h::t, class) = if class = getClass(h) then [h]@specimensFromClass(t, class)
    else specimensFromClass(t, class);

(*gets the amount of species from a indicated order*)
fun orderAmount([], order) = 0
    |   orderAmount(h::t, order) = if order = getOrder(h) then 1 + orderAmount(t, order)
    else orderAmount(t, order);

(*returns a specimen tuple from specimen list*)
fun specimenTuple(position::class::order::specie::height::weight::nill) =
    (valOf(Int.fromString(position)), class, order, specie, valOf(Real.fromString(height)), valOf(Real.fromString(weight)));

fun isDelimiter d = (d = #"," orelse d = #"\n");

fun read path =
    let
        val file = TextIO.openIn path;
        val row = valOf(TextIO.inputLine file handle e => (TextIO.closeIn; raise e))
        val _ = TextIO.closeIn file
        val l = String.tokens isDelimiter(row); 
    in l 
    end;

(*loads the data from a species .csv file*)
fun loadData path =
    let
        val input = TextIO.openIn path
        val values  = ref []
        val rawRow = ref []
    in
        while not (TextIO.endOfStream input) do
            (values := !values@[specimenTuple(String.tokens isDelimiter(valOf(TextIO.inputLine input handle e => (TextIO.closeIn; raise e))))] 
                );
        TextIO.closeIn  input;
        !values
    end;

fun classOccurrences([], class) = 0
    |   classOccurrences(h::t, class) = 
        if (getClass(h) = class) 
            then 1 + classOccurrences(t, class)
        else 
            classOccurrences(t, class);

fun classesOccurrences([]) = []
    | classesOccurrences(h::t) =
        [(getClass(h), classOccurrences(h::t, getClass(h)))]@classesOccurrences(List.filter (fn (_,y,_,_,_,_) => y <> getClass(h)) t);

fun largestSpecieName([], largest) = largest
    |   largestSpecieName(h::t, largest) = 
        if (String.size(getSpecie(h)) > String.size(largest))
            then largestSpecieName(t, getSpecie(h))
        else 
            largestSpecieName(t, largest);

fun getBySize([],floor, ceiling) = 0 
    |   getBySize(h::t, floor, ceiling) =
        if (getHeight(h) >= floor andalso getHeight(h) <= ceiling) then
            1 + getBySize(t, floor, ceiling)
        else
            getBySize(t, floor, ceiling);
end
open Utils.Parser;

let ident = a => a;

let exec = (~token_parser: 'a => string=_ => "no", parser: parser('a), cad) => {
  let (rest, res) = runParser(parser, cad);
  print_string("the rest is " ++ rest ++ "\n");
  switch (res) {
  | Ok(token) =>
    let printable = token_parser(token);
    print_string("token found " ++ printable ++ "\n");
  | Error(_) => print_string("token not found \n")
  };
};

let _ = {
  let parserA = parseSymbol("a");
  exec(parserA, "a house", ~token_parser=ident);
  /* combined */
  let parserB = parseSymbol("b");
  let parserAB = pair(parserA, parserB);
  exec(parserAB, "ab house", ~token_parser=((tok1, tok2)) =>
    tok1 ++ " - " ++ tok2
  );
};

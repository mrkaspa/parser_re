open Belt;

type parserResult('a) = (string, Result.t('a, string));

type parser('a) = string => parserResult('a);

let parseSymbol = (pattern, input) => {
  open Result;
  let size = String.length(pattern);
  let sub = String.sub(input, size, 0);
  if (pattern == sub) {
    let rest = String.sub(input, String.length(input) - size, size);
    (rest, Ok(sub));
  } else {
    (input, Error(input));
  };
};

let pair = (p1: parser('a), p2: parser('b)): parser(('a, 'b)) => {
  input => {
    Result.(
      switch (p1(input)) {
      | (rest1, Ok(res1)) =>
        switch (p2(rest1)) {
        | (rest2, Ok(res2)) => (rest2, Ok((res1, res2)))
        | _ => (input, Error(input))
        }
      | _ => (input, Error(input))
      }
    );
  };
};

let runParser = (parser: parser('a), input) => parser(input);

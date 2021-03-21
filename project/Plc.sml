(* Plc interpreter main file *)

fun run e =
    let
      val tipo = type2string(teval e [])
      val valor = val2string(eval e [])
    in
      valor^":"^tipo
    end
    handle 
        (* Exceções PLC Checker*)
          EmptySeq => "A sequência inserida não contém elementos."
        | UnknownType => "Algum dos operadores inseridos possui tipo inválido para operação."
        | NotEqTypes => "As expressões devem possuir o mesmo tipo."
        | WrongRetType => "O tipo de retorno da função não é o mesmo da expressão associada à ela."
        | DiffBrTypes => "Os valores das expressoes THEN e ELSE possuem tipos diferentes."
        | IfCondNotBool => "A condição inserida em IF não é booleana."
        | NoMatchResults => "Não existem expressões para se fazer o match."
        | MatchResTypeDiff => "O tipo de algum dos resultados do match é diferente dos outros."
        | MatchCondTypesDiff => "O tipo de alguma das opções de match difere daquele da expressao inserida no Match."
        | CallTypeMisM => "O valor passado como parametro não corresponde ao tipo do parametro da função."
        | NotFunc => "O valor inserido não é uma função."
        | ListOutOfRange => "O indice inserido ultrapassa o tamanho da lista."
        | OpNonList => "O valor inserido deve ser uma lista para que possa acessar um elemento."

        (* Exceções PLC Interpreter*)
        | Impossible => "Não foi possível avaliar esta expressão."
        | HDEmptySeq => "A sequência deve conter elementos para que hd seja utilizada."
        | TLEmptySeq => "A sequência deve conter elementos para que tl seja utilizada."
        | ValueNotFoundInMatch => "Não foi possível fazer o match para a expressão inserida."
        | NotAFunc => "A expressão inserida não é uma função."
        
        (* Exceções Environ*)
        | SymbolNotFound => "Variável não está definida.";
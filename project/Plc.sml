(* Plc interpreter main file *)

fun run e =
    let
      val tipo = type2string(teval(e)) (* Verifica se o programa é bem tipado e em caso positivo retorna o tipo de retorno*)
      val valor = val2string(eval(e))
    in
      valor^" "^tipo
    end
    handle 
        (* Exceções PLC Checker*)
          EmptySeq => "Sequencia vazia."
        | UnknownType => "Tipo de algum dos operadores e invalido."
        | NotEqTypes => "Algum dos operadores nao e um tipo para o qual a operacao de igualdade esta definida."
        | WrongRetType => "O tipo de retorno especificado para a funcao recursiva nao e o mesmo da expressao associada a ela."
        | DiffBrTypes => "Os valores das expressoes THEN e ELSE devem ser do mesmo tipo."
        | IfCondNotBool => "A expressao a ser avaliada em IF deve ser booleana."
        | NoMatchResults => "Nao existem expressoes para se fazer o match."
        | MatchResTypeDiff => "Os tipos dos resultados do match sao diferentes."
        | MatchCondTypesDiff => "Os tipos das expressoes na lista de expressoes difere-se daquele da expressao a qual se deseja fazer o match."
        | CallTypeMisM => "O valor passado como paramêtro nao corresponde ao tipo do paramêtro da funcao."
        | NotFunc => "O valor inserido nao e uma funcao."
        | ListOutOfRange => "O indice inserido ultrapassa o tamanho da lista."
        | OpNonList => "O valor inserido nao e uma lista."

        (* Exceções PLC Interpreter*)
        | Impossible => "Exception Message"
        | HDEmptySeq => "Exception Message"
        | TLEmptySeq => "Exception Message"
        | ValueNotFoundInMatch => "Exception Message"
        | NotAFunc => "Exception Message"
        
        (* Exceções Environ*)
        | SymbolNotFound => "Exception Message";
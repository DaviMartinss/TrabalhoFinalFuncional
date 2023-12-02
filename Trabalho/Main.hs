
Dispositivo mostrarItem :: Dispositivo -> String
mostrarItem (Relogio marca bateriaExtra quant ano) = 
 "Relógio da marca " ++ marca ++ 
 (if bateriaExtra then “, com bateria extra” else “”) ++ 
 ", quantidade: " ++ show quant ++ 
 ", ano: " ++ show ano ++ “\n” 

mostrarItem (Leitor marca luz memoria quant ano) = 
 "Leitor da marca " ++ marca ++ (if luz then “, com luz” else “”) ++ 
 “, memória: " ++ show memoria ++ " GB” ++ 
 ", quantidade: " ++ show quant ++ 
 ", ano: " ++ show ano ++ “\n” 

mostrarItem (Camera marca lenteExtra quant ano) = 
 "Câmera da marca " ++ marca ++ 
 (if lenteExtra then “, com lente extra” else “”) ++ 
 ", quantidade: " ++ show quant ++ 
 ", ano: " ++ show ano ++ “\n”


mostrarLista :: [Dispositivo] -> String 
mostrarLista [] = “” mostrarLista (x:xs) = mostrarItem x ++ mostrarLista xs


main :: IO () 
main = putStrLn (mostrarLista lista)


filtrarRelogios :: [Dispositivo] -> [Dispositivo] 
filtrarRelogios [] = [] 
filtrarRelogios (x:xs) = 
 case x of Relogio marca True quant ano -> x : filtrarRelogios xs _ -> filtrarRelogios xs


mostrarRelogios :: [Dispositivo] -> String 
mostrarRelogios lista = mostrarLista (filtrarRelogios lista)





-- Para Teste
data Dispositivo = Relogio String Bool Int Int
                 | Leitor String Bool Int Int Int
                 | Camera String Bool Int Int
                    deriving (Eq, Ord, Show)

item1, item2, item3, item4, item5, item6 :: Dispositivo
item1 = Relogio "Samsung" True 10 2022
item2 = Relogio "LG" False 5 2023
item3 = Leitor "Amazon" True 8 5 2020
item4 = Leitor "Asus" False 10 20 2021
item5 = Camera "Canon" False 15 2023
item6 = Camera "Samsung" True 10 2018

lista :: [Dispositivo]
lista = [item1, item2, item3, item4, item5, item6]    

mostrarItem :: Dispositivo -> String
mostrarItem (Relogio marca bateriaExtra quant ano) = 
 "Relógio da marca " ++ marca ++ 
 (if bateriaExtra then ", com bateria extra" else "") ++ 
 ", quantidade: " ++ show quant ++ 
 ", ano: " ++ show ano ++ "\n" 
mostrarItem (Leitor marca luz memoria quant ano) = 
 "Leitor da marca " ++ marca ++ (if luz then ", com luz" else "") ++ 
 ", memória: " ++ show memoria ++ " GB" ++ 
 ", quantidade: " ++ show quant ++ 
 ", ano: " ++ show ano ++ "\n" 
mostrarItem (Camera marca lenteExtra quant ano) = 
 "Câmera da marca " ++ marca ++ 
 (if lenteExtra then ", com lente extra" else "") ++ 
 ", quantidade: " ++ show quant ++ 
 ", ano: " ++ show ano ++ "\n"

-- Questão 1
mostrarLista :: [Dispositivo] -> String 
mostrarLista [] = "" 
mostrarLista (x:xs) = mostrarItem x ++ mostrarLista xs

-- main :: IO () 
-- main = putStrLn (mostrarLista lista)

-- Questão 2
verificarRelogio :: Dispositivo -> Bool
verificarRelogio relogio = case relogio of
    (Relogio marca True quant ano) -> True
    _ -> False

filtrarRelogios :: [Dispositivo] -> [Dispositivo] 
filtrarRelogios [] = [] 
filtrarRelogios relogios = filter verificarRelogio relogios

mostrarRelogiosComBateria :: [Dispositivo] -> String
mostrarRelogiosComBateria [] = "" 
mostrarRelogiosComBateria lista = mostrarLista (filtrarRelogios lista)

-- main :: IO () 
-- main = putStrLn (mostrarRelogiosComBateria lista)

-- Questão 3
verificarLeitor :: Dispositivo -> Bool
verificarLeitor leitor = case leitor of
    (Leitor marca True memoria quant ano) -> True
    _ -> False

filtrarLeitores :: [Dispositivo] -> [Dispositivo] 
filtrarLeitores [] = [] 
filtrarLeitores leitores = [leitor | leitor <- leitores, verificarLeitor leitor]

mostrarLeitoresComLuz :: [Dispositivo] -> String
mostrarLeitoresComLuz [] = "" 
mostrarLeitoresComLuz lista = mostrarLista (filtrarLeitores lista)

-- main :: IO () 
-- main = putStrLn (mostrarLeitoresComLuz lista)

-- Questão 4
verificarCamera :: Dispositivo -> Bool
verificarCamera camera = case camera of
    (Camera marca True quant ano) -> True
    _ -> False

filtrarCameras :: [Dispositivo] -> [Dispositivo] 
filtrarCameras [] = [] 
filtrarCameras cameras = filter verificarCamera cameras

mostrarCamerasComLenteExtra :: [Dispositivo] -> String
mostrarCamerasComLenteExtra [] = "" 
mostrarCamerasComLenteExtra lista = mostrarLista (filtrarCameras lista)

-- main :: IO () 
-- main = putStrLn (mostrarCamerasComLenteExtra lista)

-- Questão 5
contarEstoqueItem :: Dispositivo -> Int
contarEstoqueItem dispositivo = case dispositivo of
    Relogio _ _ quant _ -> quant
    Leitor _ _ _ quant _ -> quant
    Camera _ _ quant _ -> quant

quantidadeItensEstoque :: [Dispositivo] -> Int
quantidadeItensEstoque [] = 0
quantidadeItensEstoque (x:xs) = contarEstoqueItem x + quantidadeItensEstoque xs 

-- Questão 6
itensAntigos :: Dispositivo -> Bool
itensAntigos dispositivo = case dispositivo of
  Relogio _ _ _ ano -> ano < (2023 - 3)
  Leitor _ _ _ _ ano -> ano < (2023 - 3)
  Camera _ _ _ ano -> ano < (2023 - 3)

quantidadeItensAntigos :: [Dispositivo] -> Int
quantidadeItensAntigos lista = length ([item | item <- lista, itensAntigos item])

-- Questão 7
compararMarca :: Dispositivo -> String -> Bool
compararMarca dispositivo marca = case dispositivo of
  Relogio m _ _ _ -> marca == m
  Leitor m _ _ _ _ -> marca == m
  Camera m _ _ _ -> marca == m

mostrarItensMarca :: [Dispositivo] -> String -> String
mostrarItensMarca [] _ = ""
mostrarItensMarca lista marca = mostrarLista l
    where
        l = filter (\(dispositivo) -> compararMarca dispositivo marca) lista

main :: IO () 
main = putStrLn (mostrarItensMarca lista "Samsung")
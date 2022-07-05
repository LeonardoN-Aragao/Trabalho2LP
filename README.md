-- Gabriel Santos Fortunato: 201665074AC
-- Leonardo Nunes Aragão: 201665565C

# Trabalho2LP

## Como jogar

Para rodar o projeto precisa baixar e instalar:  
- GHCI
- Cabal-Install

link para download: https://www.haskell.org/downloads/

No terminal utilize os comandos:
$ cabal-install update
$ cabal-install random
$ ghci
$ :cd /{WorkDiretory}
$ :l main.hs
$ main

Entre com sua tentativa inicial no padrão "1234" ou "1 2 3 4"

---------------------------------------------------

## Decisões de projeto

A estratégia para validação da resposta do usuário é a seguinte: Inicialmente, 
encontra-se todos os acertos completos. Após isso, a partir das listas remanescentes
(sem considerar os acertos), é feita uma verificação em cada elemento das duas listas
de forma que a cada "match" os elementos são removidos de ambas as listas (evitando
encontrar múltiplos acertos parciais para uma só entrada). 

Para validar a entrada de cada tentativa do usuário, limpamos inicialmente a entrada
(removendo os espaços existentes) e validamos conforme as regras do jogo (de forma
que a entrada só seja aceita caso possua tamanho 4 e não possua dígitos de valor
fora do alcance entre 1 e 6)
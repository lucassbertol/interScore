# interScore

O interScore é uma aplicação web para consulta de informações do Sport Club Internacional. O backend é escrito em Haskell usando Scotty e o frontend é uma interface simples em HTML/CSS/JavaScript. A aplicação consome dados da API Football-Data.org para exibir informações sobre jogos do Brasileirão, Libertadores, calendário e elenco atual do clube, entre outros.

## Funcionalidades principais
- Exibição de jogos e resultados (Brasileirão, Libertadores, etc.)
- Calendário de partidas
- Informações do elenco atual do clube
- Consumo e atualização de dados via Football-Data.org

## Tecnologias
- Backend: Haskell (Scotty)
- Frontend: HTML, CSS, JavaScript
- Fonte de dados: Football-Data.org (API)

## Pré-requisitos
- GHC (Glasgow Haskell Compiler) ou Stack/Cabal para compilar e executar o backend
- Node (opcional, apenas se usar ferramentas de frontend locais)
- Chave de API do Football-Data.org

## Instalação rápida
1. Clone o repositório:
   git clone https://github.com/lucassbertol/interScore.git
2. Entre na pasta do projeto:
   cd interScore
3. Configure a chave da API (gerar uma fazendo login em Football-Data.org, após, inserir em
   FOOTBALL_DATA_API_KEY=**"sua_chave_aqui"** na `main.hs`)
4. Instale dependências necessárias e rode o programa:
   cabal install --lib scotty HTTP-Simple http-conduit aeson wai-extra vector text bytestring hspec QuickCheck
5. runhaskell main.hs

## Uso
- Abra o navegador em http://localhost:3000 (ou na porta configurada) para acessar a interface.
- Navegue pelas páginas para explorar


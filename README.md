# interScore

O *interScore* é uma aplicação web para consulta de informações do Sport Club Internacional, desenvolvida com **backend em Haskell** usando **`Scotty`** e frontend simples em HTML/JavaScript. O projeto consome dados da [API Football-Data.org](https://www.football-data.org/) para exibir informações sobre jogos do Brasileirão, Libertadores, calendário e elenco atual do clube, com filtros específicos em cada ocasião.

A aplicação oferece funcionalidades de:
- **Consulta de partidas** do Brasileirão e Libertadores por ano
- **Filtragem avançada** por status (finalizados/futuros), local (casa/fora) e resultado (vitória/empate/derrota)
- **Visualização de calendário** com próximas partidas
- **Informações do elenco** atual do clube

---

# Orientações para execução

**Instalação de dependências:**
```bash
# Instalar todas as dependências necessárias
cabal install --lib scotty http-simple http-conduit aeson wai-middleware-static vector text bytestring hspec QuickCheck
```

**Execução do servidor:**
```bash
# Executar o servidor principal
runhaskell main.hs
```

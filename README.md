# interScore

O *interScore* é uma aplicação web para consulta de informações do Sport Club Internacional, desenvolvida com **backend em Haskell** usando **`Scotty`** e frontend simples em HTML/JavaScript. O projeto consome dados da [API Football-Data.org](https://www.football-data.org/) para exibir informações sobre jogos do Brasileirão, Libertadores, calendário e elenco atual do clube, com filtros específicos em cada ocasião.

A aplicação oferece funcionalidades de:
- **Consulta de partidas** do Brasileirão e Libertadores por ano
- **Filtragem avançada** por status (finalizados/futuros), local (casa/fora) e resultado (vitória/empate/derrota)
- **Visualização de calendário** com próximas partidas
- **Informações do elenco** atual do clube

<img width="1917" height="937" alt="image" src="https://github.com/user-attachments/assets/a4048b0c-89cb-4838-80dc-213f4934a3f3" />

<img width="1916" height="935" alt="image" src="https://github.com/user-attachments/assets/7e646445-2ab9-439a-bac9-4c083f5db871" />

<img width="1918" height="939" alt="image" src="https://github.com/user-attachments/assets/11279434-5dd5-4f86-b327-871299769ff4" />

---

# Orientações para execução

**Instalação de dependências:**
```bash
# Instalar todas as dependências necessárias
cabal install --lib scotty HTTP-Simple http-conduit aeson wai-extra vector text bytestring hspec QuickCheck
```

**Execução do servidor:**
```bash
git clone https://github.com/lucassbertol/interScore.git
cd interScore
runhaskell main.hs

```

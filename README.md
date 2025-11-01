# Identificação

- **Autor:** Lucas Salvini Bertol  
- **Curso:** Sistemas de Informação  
- **Disciplina:** Paradigmas de Programação  

---

# Tema / Objetivo

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
cabal install --lib scotty HTTP-Simple http-conduit aeson wai-extra vector text bytestring hspec QuickCheck
```

**Execução do servidor:**
```bash
# Executar o servidor principal
git clone https://github.com/lucassbertol/interScore
runhaskell main.hs
```

**Execução dos testes:**
```bash
# Executar ambiente de testes
runhaskell spec.hs
```
---

# Demonstração do resultado final

https://github.com/user-attachments/assets/1cfb5840-74e7-40b7-a5f6-b911a12b0855

---

# Referências e créditos

- ELC117 – Paradigmas de Programação. Slides da aula: Backend Web com Scotty (Haskell). Disponível em: https://liascript.github.io/course/?https://raw.githubusercontent.com/elc117/demo-scotty-codespace-2025b/main/README.md#1

- Football-Data. (n.d.). Quickstart – Football Data API Documentation. Retrieved September 14, 2025, from https://www.football-data.org/documentation/quickstart

- HASKELL. Web.Scotty — Module documentation (scotty-0.22). Disponível em: https://hackage.haskell.org/package/scotty-0.22/docs/Web-Scotty.html

- HASKELL. Scotty — Hackage. Disponível em: https://hackage.haskell.org/package/scotty

- Build a Haskell Server with Scotty framework. 2024. Disponível em: https://www.youtube.com/watch?v=psTTKGj9G6Y

- HACKAGE. Data.Maybe: The Maybe type and associated operations. GHC Base Libraries, 2024. Disponível em: https://hackage.haskell.org/package/base/docs/Data-Maybe.html.

- Haskell JSON parsing with Aeson. Real World Haskell. Disponível em: http://book.realworldhaskell.org/

- HACKAGE. Data.Aeson.Types: JSON parsing with Maybe types. Aeson Documentation, 2024. Disponível em: https://hackage.haskell.org/package/aeson/docs/Data-Aeson-Types.html.


**Bibliotecas utilizadas:**
- `scotty`: Framework web minimalista para Haskell
- `aeson`: Biblioteca para parsing/encoding JSON
- `http-conduit`: Cliente HTTP com suporte a SSL
- `wai-middleware-static`: Middleware para servir arquivos estáticos
- `hspec`: Framework de testes para Haskell
- `QuickCheck`: Biblioteca para testes baseados em propriedades



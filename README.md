<div align="center">

<img src="https://cdn.simpleicons.org/haskell" width="60" alt="Haskell logo"/>

# interScore

**A web application for Sport Club Internacional match data and squad info**

![Haskell](https://img.shields.io/badge/Haskell-5D4F85?style=flat-square&logo=haskell&logoColor=white)
![HTML5](https://img.shields.io/badge/HTML5-E34F26?style=flat-square&logo=html5&logoColor=white)
![CSS3](https://img.shields.io/badge/CSS3-1572B6?style=flat-square&logo=css3&logoColor=white)
![Scotty](https://img.shields.io/badge/Scotty-5D4F85?style=flat-square&logo=haskell&logoColor=white)
![API](https://img.shields.io/badge/Football--Data.org-API-red?style=flat-square)

</div>

---

## 📌 Overview

**interScore** is a web application for querying information about **Sport Club Internacional**, built with a **Haskell backend** using the [Scotty](https://hackage.haskell.org/package/scotty) framework and a simple **HTML/JavaScript frontend**. It consumes data from the [Football-Data.org API](https://www.football-data.org/) to display match results, upcoming fixtures, and squad details.

Developed as part of the **Programming Paradigms** course (*Paradigmas de Programação*) — Systems of Information, UFSM.

---

## ✨ Features

- 🏆 **Brasileirão & Libertadores** — query matches by year for both competitions
- 🔍 **Advanced filtering** — filter by status (finished/upcoming), venue (home/away), and result (win/draw/loss)
- 📅 **Match calendar** — view upcoming fixtures
- 👕 **Current squad** — browse the club's active roster

---

## 🛠️ Tech Stack

| Layer | Technology |
|-------|-----------|
| Backend | Haskell + Scotty |
| JSON Parsing | Aeson |
| HTTP Client | http-conduit |
| Static Files | wai-middleware-static |
| Testing | HSpec + QuickCheck |
| Frontend | HTML5, CSS3, JavaScript |
| Data Source | Football-Data.org API |

---

## 🚀 Getting Started

### Prerequisites

- [GHC](https://www.haskell.org/ghc/) and [Cabal](https://www.haskell.org/cabal/) installed
- A free API key from [football-data.org](https://www.football-data.org/)

### 1. Clone the repository

```bash
git clone https://github.com/lucassbertol/interScore
cd interScore
```

### 2. Install dependencies

```bash
cabal install --lib scotty HTTP-Simple http-conduit aeson wai-extra vector text bytestring hspec QuickCheck
```

### 3. Run the server

```bash
runhaskell main.hs
```

The application will be available at `http://localhost:3000`.

### 4. Run tests

```bash
runhaskell spec.hs
```

---

## 📦 Dependencies

| Library | Purpose |
|---------|---------|
| [`scotty`](https://hackage.haskell.org/package/scotty) | Minimalist Haskell web framework |
| [`aeson`](https://hackage.haskell.org/package/aeson) | JSON parsing and encoding |
| [`http-conduit`](https://hackage.haskell.org/package/http-conduit) | HTTP client with SSL support |
| [`wai-middleware-static`](https://hackage.haskell.org/package/wai-middleware-static) | Middleware for serving static files |
| [`hspec`](https://hackage.haskell.org/package/hspec) | Testing framework for Haskell |
| [`QuickCheck`](https://hackage.haskell.org/package/QuickCheck) | Property-based testing library |

---

## 📚 References

- ELC117 – Paradigmas de Programação. [Backend Web com Scotty (Haskell)](https://liascript.github.io/course/?https://raw.githubusercontent.com/elc117/demo-scotty-codespace-2025b/main/README.md#1)
- [Football-Data.org — API Quickstart](https://www.football-data.org/documentation/quickstart)
- [Web.Scotty — Hackage Documentation](https://hackage.haskell.org/package/scotty-0.22/docs/Web-Scotty.html)
- [Build a Haskell Server with Scotty — YouTube](https://www.youtube.com/watch?v=psTTKGj9G6Y)
- [Data.Aeson.Types — Hackage](https://hackage.haskell.org/package/aeson/docs/Data-Aeson-Types.html)
- [Data.Maybe — Hackage](https://hackage.haskell.org/package/base/docs/Data-Maybe.html)

---

## 👤 Author

**Lucas Salvini Bertol**  
Systems of Information — Programming Paradigms

[![GitHub](https://img.shields.io/badge/GitHub-lucassbertol-181717?style=flat-square&logo=github)](https://github.com/lucassbertol)

---

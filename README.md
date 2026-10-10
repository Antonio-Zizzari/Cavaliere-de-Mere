<div align="center">

# 🎲 Cavaliere de Méré

**Il paradosso del Cavaliere de Méré verificato con una simulazione Monte Carlo in R**

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![Monte Carlo](https://img.shields.io/badge/Metodo-Monte%20Carlo-8A2BE2?style=for-the-badge)
![Probabilità](https://img.shields.io/badge/Calcolo-Probabilit%C3%A0-orange?style=for-the-badge)

</div>

---

## 📖 Panoramica

Nel XVII secolo il giocatore Antoine Gombaud, noto come **Cavaliere de Méré**, puntava su due scommesse che riteneva
equivalenti:

- **Scommessa A**: ottenere almeno un **6** lanciando **un dado 4 volte**;
- **Scommessa B**: ottenere almeno un **doppio 6** lanciando **due dadi 24 volte**.

Il suo ragionamento era che, in entrambi i casi, il rapporto tra lanci e casi possibili fosse lo stesso
(4/6 = 24/36). In pratica però la prima scommessa gli faceva vincere, la seconda perdere. Il problema fu discusso da
Pascal e Fermat ed è considerato uno dei punti di partenza della teoria della probabilità.

Lo script di questo repository riproduce le due scommesse con il **metodo Monte Carlo** e mostra che de Méré aveva
ragione a fidarsi della prima e torto a fidarsi della seconda.

## 🧮 Metodo

### Probabilità teoriche

Conviene passare all'evento complementare, cioè "nessun successo in tutti i lanci":

$$
P(A) = 1 - \left(\frac{5}{6}\right)^{4} = \frac{671}{1296} \approx 0{,}5177
$$

$$
P(B) = 1 - \left(\frac{35}{36}\right)^{24} \approx 0{,}4914
$$

La scommessa A è favorevole (sopra il 50%), la scommessa B no (sotto il 50%).

### Simulazione Monte Carlo

La funzione `cavaliere.merè(prove)` ripete l'esperimento `prove` volte. A ogni iterazione:

1. **Esperimento A**: `sample(1:6, size = 4, replace = TRUE)` simula 4 lanci di un dado; se esce almeno un 6 il
   contatore dei casi favorevoli di A aumenta di 1.
2. **Esperimento B**: vengono generati due vettori di 24 lanci (uno per dado) e sommati elemento per elemento; una somma
   pari a 12 corrisponde a un doppio 6. Se ce n'è almeno uno, il contatore di B aumenta di 1.

Alla fine la frequenza relativa $\text{casi favorevoli} / \text{prove}$ stima la probabilità di ciascuna scommessa.

| | Scommessa A | Scommessa B |
|---|:---:|:---:|
| Lanci per partita | 4 lanci di 1 dado | 24 lanci di 2 dadi |
| Evento vincente | almeno un 6 | almeno un doppio 6 |
| Probabilità teorica | ≈ **0,5177** | ≈ **0,4914** |
| Esito per il giocatore | favorevole | sfavorevole |

Lo script viene eseguito con 100, 1.000, 10.000 e 20.000 prove: con più prove le frequenze si avvicinano ai valori
teorici.

## 📁 Struttura del progetto

```
Cavaliere-de-Mere/
├── Cavaliere de Mère.R   # funzione cavaliere.merè(prove) + esecuzioni con 100, 1000, 10000, 20000 prove
└── README.md
```

## ▶️ Come eseguire

Serve un'installazione di [R](https://www.r-project.org/). Non ci sono pacchetti aggiuntivi da installare.

Il file è salvato con codifica **Latin-1** (il nome della funzione contiene la lettera `è`), quindi conviene indicarla
esplicitamente quando lo si carica:

```bash
Rscript -e 'source("Cavaliere de Mère.R", encoding = "latin1")'
```

In alternativa si può aprire il file in RStudio (_File → Reopen with Encoding… → ISO-8859-1_) ed eseguirlo con _Source_.

Per ogni numero di prove viene stampata una riga con quattro valori:

```
<casi favorevoli A> <casi favorevoli B> <frequenza A> <frequenza B>
```

Non viene fissato un seed, quindi i valori cambiano leggermente a ogni esecuzione.

## 👤 Autore

**Antonio Zizzari**, [@Antonio-Zizzari](https://github.com/Antonio-Zizzari)

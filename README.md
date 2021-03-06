# Automates et logique temporelle LTL
## Mise en route
### Dépendances
Pour le prototype :
- dune >= 2.7
- menhir >= 2.1

Pour le rapport :
- LaTeX

### Compilation
- `make` compile le prototype et le rapport.
- `make build` compile uniquement le prototype
- `make rapport` compile uniquement le rapport.
- `make clean` pour effacer les fichiers de compilation.

### Utilisation
Il faut ensuite lancer `prototype/main.exe` et passer via l'entrée standard son programme.  
Par exemple : `./prototype/main.exe < test/until`

On obtient ensuite un fichier `buchi.dot` qu'on peut convertir en pdf avec `make topdf`.


## Syntaxe
Plusieurs opérateurs sont disponibles, avec `(` et `)`.
De plus, on peut introduire des variables propositionelles dans les formules.

### Opérateurs unaires
- `!` pour la négation
- `X` pour *next*
- `F` pour *future*
- `G` pour *globally*

### Opérateurs binaires
1. `=>` pour l'implication
2. `<=>` pour l'équivalence
3. `U` pour *until*
4. `W` pour *weak until*
5. `R` pour *release*
6. `&&` pour le *et*
7. `||` pour le *ou*

### Variables et constantes
#### Constantes
Il y a deux constantes possibles :
- `1` pour *true*
- `0` pour *false*

#### Variables propositionelles
Les variables doivent avoir un nom composé uniquement de caractères minuscules.
Plus précisément, les noms vérifient `[ 'a'-'z' ]+`

### Priorité
Les opérateur unaires ont la priorité sur les opérateurs binaires.
C'est à dire que `X!a || b` s'interprète comme `X(!(a)) || b`.

Les opérateurs binaires sont donnés par ordre de priorité dans la liste précédente.
Par exemple :
- `a => b <=> c` est interprété comme `(a => b) <=> c`
- `a => b U c` comme `(a => b) U c`
- `a U b && a => b || Xb && a R b` comme `((a U b) && (a => b)) || (Xb && (a R b))`

### Exemples
Voici des exemples de formule :
- `a || b`
- `(a U b) && (F (a => b))`
- ...
D'autres exemples sont aussi présents dans le dossier `test/`.

## Sémantique
Seuls les opérateurs et constantes `1, 0, &&, ||, U, X, !` sont bien définis sémantiquement.
Les autres opérateurs sont définis en tant que *macro* (du sucre syntaxique) :
- `a => b` (implication) est un raccourci pour `!a || b`
- `a <=> b` (équivalence) pour `(a => b) && (b => a)`
- `Fa` (*future*) pour `1 U a`
- `Ga` (*globally*) pour `!(F!a)`
- `a W b` (*weak until*) pour `(a U b) || Ga`
- `a R b` (*release*) pour `!(!a U !b)`
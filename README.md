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

### Opérateur unaire
- `!` pour la négation
- `X` pour *next*
- `F` pour *future*
- `G` pour *globally*

### Opérateur binaire
1. `=>` pour l'implication
2. `<=>` pour l'équivalence
3. `U` pour *until*
4. `W` pour *weak until*
5. `R` pour *release*
6. `&&` pour le *et*
7. `||` pour le *ou*



### Priorité
Les opérateur unaires ont la priorité sur les opérateurs binaires.
C'est à dire que `X!a || b` s'interprète comme `X(!(a)) || b`.

Les opérateurs binaires sont donnés par ordre de priorité dans la liste précédente.
### Exemples
Voici des exemples de formule :
- `a || b`
- `(a U b) && (F (a => b))`
- ...
Quelques exemples sont aussi présents dans le dossier `test/`.
# Automates et logique temporelle LTL
## Mise en route
### Dépendances
- dune >= 2.7
- menhir >= 2.1
- LaTeX

### Compilation
- `make` compile le prototype et le rapport.
- `make build` compile uniquement le prototype
- `make rapport` compile uniquement le rapport.
- `make clean` pour effacer les fichiers de compilation.

### Utilisation
Il faut ensuite lancer `src/main.exe` et passer via l'entrée standard son programme.

Par exemple `prototype/main.exe < test/until`
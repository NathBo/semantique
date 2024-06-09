# Compilation

Pour compiler, utilisez `make`

L'executable est alors créé dans `_build/default/analyzer/analyzer.exe` mais peut aussi être exécuté avec certaines commandes comme `make file=myfile.c run` (d'autres commandes existent pour les utiliser d'autres paramètres).

# Utilisation

utilisez `analyser.exe fichier.c [Options]`
avec eventuellement les options suivantes :
- `--domain` pour préciser quel domaine utiliser parmi :
- - "constant"
- - "concrete"
- - "interval"
- - "sign"
- - "congruence"
- - "product"
- - "disjoint"
- - "disjoint\_congruence"
- - "disjoint\_concrete"
- - "disjoint\_sign"
- - "disjoint\_constant"
- - "disjoint\_product"
- `--backward` pour alterner des itérations en avant et en arrière

# Tests
Pour lancer les tests, utilisez `make test` puis visualisez le résultat dans `results/index.html`.

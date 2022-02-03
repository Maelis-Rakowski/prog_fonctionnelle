object TP2Ex1:

  /* On souhaite définir une table qui associe un entier à des chaînes de caractères (par exemple pour compter le
   * nombre d'occurrences de chaque mot dans un texte). On utilise l'aspect fonctionnel du langage: une table est
   * une fonction de type 'String => Int'.
   *
   * La définition suivante créé simplement un alias, c'est à dire une notation pour faciliter la lecture des types de
   * fonctions. Chaque occurrence du mot 'Table' est donc interchangeable avec 'String => Int'. */
  type Table = String => Int

  def valueInTable(t: Table, s: String): Int = t(s)

  /* Définissez la table vide (qui associe zéro à chaque mot) */
  def emptyTable: Table = (mot : String) => 0;

  /* Définissez une fonction pour créer une nouvelle table, qui associe la valeur n à la chaîne s, et qui est similaire
   * à t pour toutes les autres chaînes. */
  def updateTable(t: Table, s: String, n: Int): Table = (mot : String) => if(mot == s) then n else t(s) ;


object TP2Ex2:
  /* On peut aussi représenter les ensembles (ici, ensembles d'entiers) par une fonction booléenne qui associe 'true'
   * à un élément si et seulement si cet élement est dans l'ensemble.
   *
   * Cette approche permet notamment de modéliser de manipuler des ensembles infinis (l'ensemble de tous les entiers,
   * l'ensemble de tous les entiers pairs...). */
  type IntSet = Int => Boolean

  def elem(x: Int, s: IntSet): Boolean = s(x)

  /* Définir les ensembles vide et singleton, ainsi que les opérations ensemblistes suivantes. */
  def emptySet: IntSet = (num : Int) => false;

  def singleton(n: Int): IntSet = (num : Int ) => n == num

  def union(s1: IntSet, s2: IntSet): IntSet = (num : Int) => s1(num) || s2(num)

  def intersection(s1: IntSet, s2: IntSet): IntSet = (num : Int) => s1(num) && s2(num)

  def complement(s: IntSet): IntSet = (num : Int) =>  !s(num)

  def difference(s1: IntSet, s2: IntSet): IntSet = (num : Int) => s1(num) && !s2(num)

  /* Pourquoi est-il impossible de définir une fonction qui indique si deux ensembles IntSet sont égaux? Quelles autres
   * opérations ensemblistes ne peut-on pas définir pour IntSet? */


object TP2Ex3:

  /* Sur le même principe, définissez le type des ensembles de pairs ordonnées d'entiers en remplacant le mot-clé
   * 'Nothing' par une définition appropriée. */
  type PairOfIntSet = (Int, Int) => Boolean

  def elem(n: Int, m: Int, s: PairOfIntSet): Boolean = s(n, m)

  /* Définissez le produit cartésien de deux IntSet */
  def cartesianProduct(s1: TP2Ex2.IntSet, s2: TP2Ex2.IntSet): PairOfIntSet = (n1 : Int, n2 : Int) => (s1(n1) && s2(n2)) || (s1(n2) && s2(n1))


object TP2Ex4:

  /* Ce type de donnée représente des fonctions sur les nombres réels (ou plus exactement les nombres flottants) */
  type RealFunction = Double => Double

  /* Définissez l'évaluation d'une fonction f en un point x et les opérations sur les fonctions */
  def eval(f: RealFunction, x: Double): Double = f(x);

  /* -f */
  def negate(f: RealFunction): RealFunction = (x : Double) => -f(x)

  /* 1/f */
  def invert(f: RealFunction): RealFunction = (x : Double) => 1/f(x)

  /* f1 + f2 */
  def add(f1: RealFunction, f2: RealFunction): RealFunction = (x : Double) => f1(x) + f2(x)

  /* f1 - f2 */
  def sub(f1: RealFunction, f2: RealFunction): RealFunction = (x : Double) => f1(x) - f2(x)

  /* f1 * f2 */
  def mult(f1: RealFunction, f2: RealFunction): RealFunction = (x : Double) => f1(x) * f2(x)

  /* f1 / f2 */
  def div(f1: RealFunction, f2: RealFunction): RealFunction = (x : Double) => f1(x) / f2(x)

  /* f2 ∘ f1 (composition de fonctions) */
  def compose(f1: RealFunction, f2: RealFunction): RealFunction = (x : Double) => f2(f1(x))

  /* la fonction constante en c */
  def constant(c: Double): RealFunction = (x : Double) => c

  /* la fonction associée au monôme coef.x^exponent */
  def monomial(coef: Double, exponent: Int): RealFunction = (x : Double) => coef* Math.pow(x, exponent)

  /* la dérivée de f, approximée par la méthode de la différence centrale:
   * f'(x) est approximée par (f(x + delta) - f(x - delta)) / 2.delta */
  def approximateDerivative(f: RealFunction, delta: Double): RealFunction = (x : Double) => (f(x + delta) - f(x - delta)) / (2*delta)

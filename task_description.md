# Zadanie 6: Przelewanka

Masz dane $n$ szklanek, ponumerowanych od $1$ do $n$, o pojemnościach odpowiednio $x_1, x_2, \ldots, x_n$.  Początkowo wszystkie szklanki są puste. Możesz wykonywać następujące czynności:

- **nalać** do wybranej szklanki do pełna wody z kranu, 
- **wylać** całą wodę z wybranej szklanki do zlewu, 
- **przelać** wodę z jednej szklanki do drugiej — jeżeli się zmieści, to przelewasz całą wodę, a jeżeli nie, to tyle żeby druga szklanka była pełna.

Twoim celem jest uzyskanie takiej sytuacji, że w każdej szklance jest określona ilość wody, odpowiednio $y_1, y_2, \ldots, y_n$. 

Napisz procedurę `przelewanka : (int * int) array -> int`, która mając daną tablicę par liczb `[|(x1, y1); (x2, y2); ...; (xn, yn)|]` **wyznaczy minimalną liczbę czynności potrzebnych do uzyskania opisanej przez nie sytuacji**. Jeżeli jej uzyskanie nie jest możliwe, to poprawnym wynikiem jest `-1`. 

Możesz założyć, że $0 \leq n$, oraz $0 \leq y_i \leq x_i$ dla $i = 1, 2, \ldots, n$.

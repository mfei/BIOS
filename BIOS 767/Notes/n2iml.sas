%let progname = n2iml.sas;
%let pdfout   = n2iml.pdf;
* xref:
* input:
* output:
* does: - Introduction to IML
**************************************************;
title1 "&progname: Introduction to IML ";
ods pdf file = "&pdfout";
options nocenter errors=3;
**************************************************;
proc iml;

a = {1 3 5};               * vector;
b = {1 1 1, 1 2 4, 1 3 9}; * matrix;
c = {1 7 5, 2 9 2, 3 3 3}; * matrix;
pi = 4*atan(1);         * 3.141592...;
print a  b  c  pi;

r = 1:10;               * sequence of values;
print r;
r = do(1, 7, 2);        * sequence of values;
print r;
r = do(1, -7, -2);      * sequence of values;
print r;

eye = i(5);             * the identity matrix;
zeros = j(5, 3, 0);     * a matrix of 0's;
ones = j(2, 4, 1);      * a matrix of 1's;
print eye, zeros, ones;

a   = a`;               * transpose (backward single quote);
ba1 = b || a;           * 4th column <- a;
ba2 = b // a`;          * 4th row <- a';
b22 = b[2,2];           * one element;
sub = b[1:2, 2:3];      * submatrix;
bd  = block(b, c);      * block diagonal matrix;
rep = repeat(c, 2, 3);  * repeat a matrix;
print a, ba1, ba2, b22, sub, bd, rep;

d1 = diag(c);           * extract the diagonal elements;
d2 = diag(a);           * construct a diagonal matrix;
print d1, d2;

a1 = b+c;
a2 = b-c;
a3 = b*a;
a4 = b#a;
a5 = b/c;               * elementwise divide;
a6 = b##2;              * elementwise power;
a7 = (b=c);             * elementwise comparison;
a8 = (b>c);             * elementwise >;
print a1, a2, a3, a4, a5, a6, a7;

a1 = exp(b);
a2 = sqrt(b);
x = solve(b, a);           * solve (b x = a) for x;
eval = eigval(b);          * eigenvalues;
call eigen(eval, evec, b); * eigenvalues and eigenvectors;
print x, eval, evec;

show names;             * list all variables;

seed = j(5, 3, 13579);  * seed matrix;
u    = ranuni(seed);    * random uniform(0,1);
n    = rannor(seed);    * random N(0,1);
print u, n;

s = 0;
do  i=1 to 10; s = s + i; end;
print s;

x = do(0, 1, 0.1)`;
y = x#x;
print x y;

x = round(ranuni(seed)*100);
min1 = min(x);                  * min;
min2 = x[><];                   * min;
rowmin = x[,><];                * row min;
colmin = x[><,];                * column min;
print x, min1, min2, rowmin, colmin;

i  = x[>:<];                    * index of min, row-major numbering;
ir = x[,>:<];
ic = x[>:<,];
print i, ir, ic;

max1 = max(x);                  * max;
max2 = x[<>];                   * max;
i    = x[<:>];                  * index of max;
print max1, max2, i;

xbar = x[:];                    * mean;
rowmean= x[,:];                 * row mean;
colmean= x[:,];                 * column mean;
s = sum(x);
cs= cusum(x);                   * cumulative sum, row-major;
print xbar, rowmean, colmean, s, cs;

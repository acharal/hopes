% ISO Prolog
% precedence and assosiativity of default prolog operators

:- op(1200, xfx, ':-').     % rule implication
:- op(1200, xfx, '-->').    % if-then-else
:- op(1200,  fx, ':-').     % goal implication
:- op(1200,  fx, '?-').     % ?
:- op(1100, xfy, ';').      % disjunction
:- op(1050, xfy, '->').
:- op(1000, xfy, ',').      % conjunction
:- op( 700, xfx, '=').      % unification
:- op( 700, xfx, '\\=').
:- op( 700, xfx, '==').
:- op( 700, xfx, '\\==').
:- op( 700, xfx, '@<').
:- op( 700, xfx, '@=<').
:- op( 700, xfx, '@>').
:- op( 700, xfx, '@>=').
:- op( 700, xfx, 'is').     % arithmetic evaluation
:- op( 700, xfx, '=:=').    % equal
:- op( 700, xfx, '=\\=').   % not equal
:- op( 700, xfx, '<').      % less
:- op( 700, xfx, '=<').     % less equal
:- op( 700, xfx, '>').      % greater
:- op( 700, xfx, '>=').     % greater equal
:- op( 700, xfx, '=..').    % conversion of atom to list
:- op( 500, yfx, '+').      % addition
:- op( 500, yfx, '-').      % substraction
:- op( 500, yfx, '/\\').    % bitwise and
:- op( 500, yfx, '\\/').    % bitwise or
:- op( 400, yfx, '*').      % multiplication
:- op( 400, yfx, '/').      % Pred/Arity - floating point division
:- op( 400, yfx, '//').     % integer division
:- op( 400, yfx, 'rem').    % remainder
:- op( 400, yfx, 'mod').    % modulo
:- op( 400, yfx, '<<').     % bitswift leftwards
:- op( 400, yfx, '>>').     % bitswift rightwards
:- op( 200, xfx, '**').     % exponent
:- op( 200, xfy, '^').
:- op( 200,  fy, '\\').     % bitwise complement
:- op( 200,  fy, '-').      % sign reversal
:- op( 100, xfx, '@').
:- op(  50, xfx, ':').      % module specification of a predicate Module:Pred

:- op( 600,  fy, '\\+').    % negation as failure

:- op(1100, xfx,'|').       % or alternative to ';'

:- op( 701,  fy, 'dynamic').
:- op( 701,  fy, 'multifile').
:- op( 701,  fy, 'meta_predicate').

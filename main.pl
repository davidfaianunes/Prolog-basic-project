/*

O seguinte codigo foi desenvolvido por David Faia Nunes (ist1102890).

Objetivo: Este codigo visa a resolucao de puzzles hashi.

Sidenote: Quando o termo slot e mencionado nos comentarios, refiro-me a uma posicao do puzzle.
*/



% -PREDICADOS AUXILIARES-

% Os predicados "inteiro_positivo" e "inteiro_nao_negativo" verifica-se quando o parametro nao e 0.

inteiro_positivo(X) :-
	integer(X),
	X > 0.

inteiro_nao_negativo(X) :-
	integer(X),
	X>0;
	X == 0.






% -PREDICADOS PRINCIPAIS-





% 	2.1-PREDICADO extrai_ilhas_linha(L, Pontes, Ilhas), em que L e um inteiro positivo,
% correspondente ao numero de uma linha e Pontes e uma lista correspondente a uma linha
% de um puzzle, significa que Ilhas e a lista ordenada (ilhas da esquerda para a direita)
% cujos elementos sao as ilhas da linha que contem os slots em Pontes.





extrai_ilhas_linha(L, Pontes, Ilhas) :-											% o quarto parametro de extrai_ilhas_linha/4 corresponde a um contador
	extrai_ilhas_linha(L, Pontes, Ilhas, 1).									% (para obter o no. da coluna da ilha), que permite indexar os slots

% no caso de haver apenas slots com o numero 0, ou no caso de Pontes==[], Ilhas tambem sera uma lista vazia:

extrai_ilhas_linha(L, Pontes, []) :-

	findall(A, (member(A, Pontes), inteiro_nao_negativo(A)), PontesValidas),
	Pontes == PontesValidas,													% para verificar se todos os slots das Pontes sao validos

	inteiro_positivo(L),														% para verificar se o numero da linha e valido


	findall(A, (member(A, Pontes), inteiro_positivo(A)), PontesNaoNulas),		% verificar se todos os slots da linha contem 0 - ou seja, nao ha ilhas
	PontesNaoNulas == [], !.

extrai_ilhas_linha(L, Pontes, [], _) :-

	findall(A, (member(A, Pontes), inteiro_nao_negativo(A)), PontesValidas),
	Pontes == PontesValidas,													% para verificar se todos os slots das Pontes sao validos

	inteiro_positivo(L),														% para verificar se o numero da linha e valido


	findall(A, (member(A, Pontes), inteiro_positivo(A)), PontesNaoNulas),		% verificar se todos os slots da linha contem 0 - ou seja, nao ha ilhas
	PontesNaoNulas == [], !.




extrai_ilhas_linha(L, Pontes, Ilhas, IndexLinha) :-

	findall(A, (member(A, Pontes), inteiro_nao_negativo(A)), PontesValidas),
	Pontes == PontesValidas,													% verificar se todos os slots das Pontes sao validos

	inteiro_positivo(L),														% verificar se o numero da linha e valido


	\+ Pontes == [],															% verificar que ainda estamos a analizar um conjunto nao vazio de slots


	Pontes = [Ponte|OutrasPontes],
	\+ Ponte == 0,
	Ilha = ilha(Ponte, (L, IndexLinha)),
	NovoIndexLinha is IndexLinha + 1,											% incrementar o IndexLinha por 1, para contar os numeros das colunas dos slots
	extrai_ilhas_linha(L, OutrasPontes, OutrasIlhas, NovoIndexLinha),
	append([Ilha], OutrasIlhas, Ilhas);											% juntar a ilha analizada neste passo com as ilhas recursivamente obtidas

	% caso o slot contenha "0"

	\+ Pontes == [],

	findall(A, (member(A, Pontes), inteiro_nao_negativo(A)), PontesValidas),
	Pontes == PontesValidas,													% verificar se todos os slots das Pontes sao validos

	inteiro_positivo(L),														% verificar se o numero da linha e valido

	Pontes = [Ponte|OutrasPontes],
	Ponte == 0,
	NovoIndexLinha is IndexLinha + 1,											% incrementar o IndexLinha por 1, para contar os numeros das colunas dos slots
	extrai_ilhas_linha(L, OutrasPontes, Ilhas, NovoIndexLinha).					% obter ilhas, recursivamente, ate chegar a Pontes == []





% 	2.2-PREDICADO ilhas(Puzzle, Ilhas), em que Puzzle e um puzzle, significa que Ilhas e a lista ordenada
% (ilhas da esquerda para a direita e de cima para baixo) cujos elementos sao as ilhas do
% Puzzle.





ilhas(Puzzle, Ilhas) :-
	ilhas(Puzzle, Ilhas, 1, []).												% o 3o parametro de ilhas/4 corresponde a um contador (para obter o no.
																				% da linha analizada de momento); o 4o parametro corresponde a um "contentor"
																				% para as ilhas registradas nos passos anteriores da recursao

ilhas(Puzzle, Ilhas, NumeroLinha, IlhasAnteriores) :-
	\+ Puzzle == [],
	Puzzle = [Linha|OutrasLinhas],
	extrai_ilhas_linha(NumeroLinha, Linha, IlhasDaLinha),
	append(IlhasAnteriores, IlhasDaLinha, IlhasTodas),
	NovoNumeroLinha is NumeroLinha + 1,
	ilhas(OutrasLinhas, Ilhas, NovoNumeroLinha, IlhasTodas), !.					% assim que uma solucao e encontrada, para a procura por valores para Ilha
																				% para evitar duplicados da solucao correta



ilhas([], Ilhas, _, Ilhas).														% se nao houver mais linhas a analizar(o restante do puzzle a analizar == []),
																				% as ilhas anteriormente acumuladas, correspondem as ilhas todas.




% 	2.3-PREDICADO vizinhas(Ilhas, Ilha, Vizinhas), em que Ilhas e a lista de ilhas de um puzzle
% e Ilha e uma dessas ilhas, significa que Vizinhas e a lista ordenada (ilhas de cima para
% baixo e da esquerda para a direita ) cujos elementos sao as ilhas vizinhas de Ilha.





vizinhas(Ilhas, Ilha, Vizinhas) :-
	
	member(Ilha, Ilhas),

	vizinhaCima(Ilhas, Ilha, VizinhaCima),
	vizinhaEsquerda(Ilhas, Ilha, VizinhaEsquerda),								% a ordem certa e sempre vizinhaCima, vizinhaEsq, vizinhaDir, vizinhaBaixo.
	vizinhaDireita(Ilhas, Ilha, VizinhaDireita),
	vizinhaBaixo(Ilhas, Ilha, VizinhaBaixo),

	append(VizinhaCima,VizinhaEsquerda,JuntaCimaEsq),
	append(JuntaCimaEsq,VizinhaDireita, JuntaCimaEsqDir),
	append(JuntaCimaEsqDir,VizinhaBaixo, Vizinhas).

% 	predicados auxiliares para obter cada uma das vizinhas - no caso de nao existir uma certa vizinha, entao vizinhaX = [], para nao alterar a lista Vizinhas, quando juntar

%	no caso da vizCima e vizEsq, sao os ultimos elementos(numa lista ordenada) dos elementos anteriores da mesma coluna e linha, respetivamente

vizinhaCima(Ilhas, ilha(_,(Linha,Coluna)), VizinhaCima) :-
	findall(ilha(Pontes, (LinhaVizinha, Coluna)), (member(ilha(Pontes, (LinhaVizinha, Coluna)), Ilhas), LinhaVizinha < Linha), VizinhasCima),
	length(VizinhasCima, NumeroVizinhasCima),
	nth1(NumeroVizinhasCima, VizinhasCima, VizinhaCimaSemParenteses),
	VizinhaCima = [VizinhaCimaSemParenteses];

	findall(ilha(Pontes, (LinhaVizinha, Coluna)), (member(ilha(Pontes, (LinhaVizinha, Coluna)), Ilhas), LinhaVizinha < Linha), VizinhasCima),
	length(VizinhasCima, NumeroVizinhasCima),
	0 is NumeroVizinhasCima,
	VizinhaCima = [].

vizinhaEsquerda(Ilhas, ilha(_,(Linha,Coluna)), VizinhaEsquerda) :-
	findall(ilha(Pontes, (Linha, ColunaVizinha)), (member(ilha(Pontes, (Linha, ColunaVizinha)), Ilhas), ColunaVizinha < Coluna), VizinhasEsquerda),
	length(VizinhasEsquerda, NumeroVizinhasEsquerda),
	nth1(NumeroVizinhasEsquerda, VizinhasEsquerda, VizinhaEsquerdaSemParenteses),
	VizinhaEsquerda = [VizinhaEsquerdaSemParenteses];

	findall(ilha(Pontes, (Linha, ColunaVizinha)), (member(ilha(Pontes, (Linha, ColunaVizinha)), Ilhas), ColunaVizinha < Coluna), VizinhasEsquerda),
	length(VizinhasEsquerda, NumeroVizinhasEsquerda),
	0 is NumeroVizinhasEsquerda,
	VizinhaEsquerda = [].

%	no caso da vizDir e vizBaixo, sao os ultimos primeiros(numa lista ordenada) dos elementos posteriores da mesma coluna e linha, respetivamente

vizinhaDireita(Ilhas, ilha(_,(Linha,Coluna)), VizinhaDireita) :-
	findall(ilha(Pontes, (Linha, ColunaVizinha)), (member(ilha(Pontes, (Linha, ColunaVizinha)), Ilhas), ColunaVizinha > Coluna), VizinhasDireita),
	nth1(1, VizinhasDireita, VizinhaDireitaSemParenteses),
	VizinhaDireita = [VizinhaDireitaSemParenteses];

	findall(ilha(Pontes, (Linha, ColunaVizinha)), (member(ilha(Pontes, (Linha, ColunaVizinha)), Ilhas), ColunaVizinha > Coluna), VizinhasDireita),
	length(VizinhasDireita, NumeroVizinhasDireita),
	0 is NumeroVizinhasDireita,
	VizinhaDireita = [].

vizinhaBaixo(Ilhas, ilha(_,(Linha,Coluna)), VizinhaBaixo) :-
	findall(ilha(Pontes, (LinhaVizinha, Coluna)), (member(ilha(Pontes, (LinhaVizinha, Coluna)), Ilhas), LinhaVizinha > Linha), VizinhasBaixo),
	nth1(1, VizinhasBaixo, VizinhaBaixoSemParenteses),
	VizinhaBaixo = [VizinhaBaixoSemParenteses];

	findall(ilha(Pontes, (LinhaVizinha, Coluna)), (member(ilha(Pontes, (LinhaVizinha, Coluna)), Ilhas), LinhaVizinha > Linha), VizinhasBaixo),
	length(VizinhasBaixo, NumeroVizinhasBaixo),
	0 is NumeroVizinhasBaixo,
	VizinhaBaixo = [].





% 	2.4-PREDICADO estado(Ilhas, Estado), em que Ilhas e a lista de ilhas de um puzzle, significa que
% Estado e a lista ordenada cujos elementos sao as entradas referentes a cada uma das
% ilhas de Ilhas;
%	Uma entrada corresponde a uma lista em que: o 1o elemento e uma ilha; o 2o elemento e lista de vizinhas dessa ilha;
% o 3o elemento e a lista de pontes da ilha - esta lista e vazia no estado inicial.





estado([], []).																% estado final para concluir recursao (para estado/2)

estado(Ilhas, Estado) :-													% o 1o parametro(Ilhas) e utilizado na recursao e vai se reduzindo ate ser [], enquanto que
	estado(Ilhas, Estado, Ilhas).											% o 3o parametro(Ilhas) nao se altera, permitindo sempre analizar todas as ilhas do puzzle

estado([], [], _).															% estado final para concluir recursao (para estado/3)

estado([Ilha|OutrasIlhas], Estado, IlhasTodas) :-
	vizinhas(IlhasTodas, Ilha, Vizinhas),
	Entrada = [[Ilha, Vizinhas, []]],										% Entrada dentro duma nested list, para aquando do junta, Entrada ser um elemento da lista Estado
	estado(OutrasIlhas, RestoEstado, IlhasTodas),
	append(Entrada, RestoEstado, Estado).





% 	2.5-PREDICADO posicoes_entre(Pos1, Pos2, Posicoes), em que Pos1 e Pos2 sao posicoes, significa que
% Posicoes e a lista ordenada de posicoes entre Pos1 e Pos2 (excluindo Pos1 e Pos2). Se Pos1 e Pos2
% nao pertencerem a mesma linha ou a mesma coluna, o resultado e false.





posicoes_entre((L, C1), (L, C2), PosicoesEntre) :-
	inteiro_nao_negativo(L),												% analizar se as coorndenadas das posicoes (2 primeiros parametros) sao validas
	inteiro_nao_negativo(C1),
	inteiro_nao_negativo(C2),
	C1 < C2,
	IndexC is C1 + 1,

	procura_posicoes_linha((L, C1), (L, C2), IndexC, PosicoesEntre, []);	% 3o parametro - index para percorrer todas as posicoes intermedias;
																			% 5o parametro - "deposito" para as posicoes ja analizadas (inicialmente vazio)
	C1 > C2,																
	posicoes_entre((L, C2), (L, C1), PosicoesEntre).						% se a 2a posicao foi anterior a 1a posicao, inverter 2 primeiros parametros



posicoes_entre((L1, C), (L2, C), PosicoesEntre) :-
	inteiro_nao_negativo(C),												% analizar se as coorndenadas das posicoes (2 primeiros parametros) sao validas
	inteiro_nao_negativo(L1),
	inteiro_nao_negativo(L2),
	L1 < L2,
	IndexL is L1 + 1,

	procura_posicoes_coluna((L1, C), (L2, C), IndexL, PosicoesEntre, []);	% 3o parametro - index para percorrer todas as posicoes intermedias;
																			% 5o parametro - "deposito" para as posicoes ja analizadas (inicialmente vazio)
	L1 > L2,
	posicoes_entre((L2, C), (L1, C), PosicoesEntre).						% se a 2a posicao foi anterior a 1a posicao, inverter 2 primeiros parametros



% se Pos1 e Pos2 estiverem na mesma linha:

procura_posicoes_linha((L, C1), (L, C2), IndexC, PosicoesEntre, DepositoPosicoes):-
	\+ IndexC == C2,
	append(DepositoPosicoes, [(L,IndexC)], PosicoesRecolhidas),
	NovoIndexC is IndexC + 1,
	procura_posicoes_linha((L, C1), (L, C2), NovoIndexC, PosicoesEntre, PosicoesRecolhidas).


procura_posicoes_linha((L, _), (L, C2), C2, DepositoPosicoes, DepositoPosicoes).



% se Pos1 e Pos2 estiverem na mesma coluna:

procura_posicoes_coluna((L1, C), (L2, C), IndexL, PosicoesEntre, DepositoPosicoes):-
	\+ IndexL == L2,
	append(DepositoPosicoes, [(IndexL,C)], PosicoesRecolhidas),
	NovoIndexL is IndexL + 1,
	procura_posicoes_coluna((L1, C), (L2, C), NovoIndexL, PosicoesEntre, PosicoesRecolhidas).


procura_posicoes_coluna((_, C), (L2, C), L2, DepositoPosicoes, DepositoPosicoes).





% 	2.6-PREDICADO cria_ponte(Pos1, Pos2, Ponte), em que Pos1 e Pos2 sao 2 posicoes, significa
% que Ponte e uma ponte entre essas 2 posicoes.





cria_ponte((L,C1), (L,C2), Ponte) :-		% no caso das posicoes estarem na mesma linha
	C1 < C2,
	Ponte = ponte((L,C1),(L,C2));

	C1 > C2,
	Ponte = ponte((L, C2), (L, C1)).

cria_ponte((L1, C), (L2,C), Ponte) :-		% no caso das posicoes estarem na mesma coluna
	L1 < L2,
	Ponte = ponte((L1,C),(L2,C));

	L1 > L2,
	Ponte = ponte((L2,C),(L1,C)).





%	2.7-PREDICADO caminho_livre((L1,C1),(L2,C2), EntrePos1Pos2, (LV1,CV1), (LV2,CV2)),
% em que (L1,C1) e (L2,C2) sao posicoes, EntrePos1Pos2 e a lista ordenada de posicoes entre as ilhas das posicoes anteriores,
% (LV1,CV1) e uma ilha, e (LV2,CV2) e uma das suas vizinhas, significa que a adicao da ponte ponte(Pos1, Pos2) nao faz com que
% (LV1,CV1) e (LV2,CV2) deixem de ser vizinhas.





caminho_livre((L1,C1),(L2,C2), EntrePos1Pos2, ilha(_,(L1,C1)), ilha(_,(L2,C2))):-		% a ponte nao tapa o caminho entre as ilhas A e B, se a ponte for entre A e B
	posicoes_entre((L1,C1),(L2,C2), EntrePos1Pos2), !.

caminho_livre((L1,C1),(L2,C2), EntrePos1Pos2, ilha(_,(L2,C2)), ilha(_,(L1,C1))):-		% a ponte nao tapa o caminho entre as ilhas B e A, se a ponte for entre A e B
	posicoes_entre((L2,C2),(L1,C1), EntrePos1Pos2), !.

caminho_livre((L1,C1),(L2,C2), EntrePos1Pos2, ilha(_,(LV1,CV1)), ilha(_,(LV2,CV2))):- 	% (nota, por ex.: LV1 = LinhaVizinha1)
	posicoes_entre((L1,C1),(L2,C2), EntrePos1Pos2),
	posicoes_entre((LV1,CV1),(LV2,CV2), EntreAtuaisVizinhas),
	findall(A, (member(A,EntreAtuaisVizinhas),member(A,EntrePos1Pos2)), Interseccao),
	Interseccao == [].													
																		% logo, o predicado verifica-se, se nao houver nenhuma posicao entre as anteriormente vizinhas,
																		% e em EntrePos1Pos2.




%	2.8-PREDICADO actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada), em que Pos1 e Pos2 sao
% as posicoes entre as quais ira ser adicionada uma ponte, Posicoes e a lista ordenada de posicoes entre
% Pos1 e Pos2, e Entrada e uma entrada, significa que Nova_Entrada e igual a
% Entrada, excepto no que diz respeito a lista de ilhas vizinhas; esta deve ser actualizada,
% removendo as ilhas que deixaram de ser vizinhas, apos a adicao da ponte.





% o 6o parametro de actualiza_vizinhas_entrada/6 e um deposito para as ilhas que se verificam ainda vizinhas de ilha(P,(L,C)), apos a adicao da ponte

actualiza_vizinhas_entrada(Pos1, Pos2, EntrePos1Pos2, [ilha(P,(L,C)),Vizinhas,Pontes], [ilha(P,(L,C)),AindaVizinhas,Pontes]) :-
	actualiza_vizinhas_entrada(Pos1, Pos2, EntrePos1Pos2, [ilha(P,(L,C)),Vizinhas,Pontes], [ilha(P,(L,C)),AindaVizinhas,Pontes], []).


actualiza_vizinhas_entrada(Pos1, Pos2, EntrePos1Pos2, [ilha(P,(L,C)),Vizinhas,Pontes], [ilha(P,(L,C)),AindaVizinhas,Pontes], DepositoVizinhas) :-

	Vizinhas = [Vizinha|OutrasVizinhas],
	Vizinha = ilha(_,(LVizinha, CVizinha)),
	caminho_livre(Pos1,Pos2,EntrePos1Pos2,ilha(_,(L,C)),ilha(_,(LVizinha,CVizinha))),
	append(DepositoVizinhas, [Vizinha], VizinhasReunidas),
	VizinhasReunidas = [_|_],	% ou seja corresponde a uma lista de pelo menos um elemento
	actualiza_vizinhas_entrada(Pos1, Pos2, EntrePos1Pos2, [ilha(P,(L,C)),OutrasVizinhas,Pontes], [ilha(P,(L,C)),AindaVizinhas,Pontes], VizinhasReunidas);

	Vizinhas = [Vizinha|OutrasVizinhas],
	Vizinha = ilha(_,(LVizinha, CVizinha)),
	caminho_livre(Pos1,Pos2,EntrePos1Pos2,ilha(_,(L,C)),ilha(_,(LVizinha,CVizinha))),
	append(DepositoVizinhas, [Vizinha], VizinhasReunidas),
	\+ VizinhasReunidas = [_|_],	% ou seja nao corresponde a uma lista de pelo menos um elemento
	actualiza_vizinhas_entrada(Pos1, Pos2, EntrePos1Pos2, [ilha(P,(L,C)),OutrasVizinhas,Pontes], [ilha(P,(L,C)),AindaVizinhas,Pontes], [VizinhasReunidas]);

	Vizinhas = [Vizinha|OutrasVizinhas],
	Vizinha = ilha(_,(LVizinha, CVizinha)),
	\+ caminho_livre(Pos1,Pos2,EntrePos1Pos2,ilha(_,(L,C)),ilha(_,(LVizinha,CVizinha))),
	actualiza_vizinhas_entrada(Pos1, Pos2, EntrePos1Pos2, [ilha(P,(L,C)),OutrasVizinhas,Pontes], [ilha(P,(L,C)),AindaVizinhas,Pontes], DepositoVizinhas).

% quando nao houver mais ilhas a analizar, podemos concluir que as ilhas vizinhas na nova entrada, equivalem as que estao atualmente no DepositoVizinhas

actualiza_vizinhas_entrada(_, _, _, [ilha(P,(L,C)),[],Pontes], [ilha(P,(L,C)),DepositoVizinhas,Pontes], DepositoVizinhas).





%	2.9-PREDICADO actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado),
% em que Estado e um estado (ver Seccao 2.4), Pos1 e Pos2 sao as posicoes entre as
% quais foi adicionada uma ponte, significa que Novo_estado e o estado que se obtem de
% Estado apos a actualizacao das ilhas vizinhas de cada uma das suas entradas.





actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, NovoEstado):-
	Estado = [Entrada|OutrasEntradas],
	NovoEstado = [NovaEntrada|OutrasNovasEntradas],
	posicoes_entre(Pos1, Pos2, EntrePos1Pos2),
	actualiza_vizinhas_entrada(Pos1, Pos2, EntrePos1Pos2, Entrada, NovaEntrada),
	actualiza_vizinhas_apos_pontes(OutrasEntradas, Pos1, Pos2, OutrasNovasEntradas),
	append([NovaEntrada], OutrasNovasEntradas, NovoEstado).

actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, NovoEstado):-
	Estado = [Entrada],
	NovoEstado = [NovaEntrada],
	posicoes_entre(Pos1, Pos2, EntrePos1Pos2),
	actualiza_vizinhas_entrada(Pos1, Pos2, EntrePos1Pos2, Entrada, NovaEntrada).





%	2.10-PREDICADO ilhas_terminadas(Estado, Ilhas_term), em que Estado e um estado, 
% significa que Ilhas_term e a lista de ilhas que ja tem todas as pontes associadas,
% designadas por ilhas terminadas. Se a entrada referente a uma ilha for [ilha(N_pontes,
% Pos), Vizinhas, Pontes], esta ilha esta terminada se N_pontes for diferente de
% 'X' (a razao para esta condicao ficara aparente mais a frente) e o comprimento da lista
% Pontes for N_pontes .





ilhas_terminadas(Estado, IlhasTerm) :-
	ilhas_terminadas(Estado, IlhasTerm, []).

ilhas_terminadas(Estado, IlhasTerm, DepositoIlhas) :-
	Estado = [Entrada|OutrasEntradas],
	Entrada = [ilha(NumPontes,Posicao), _, Pontes],
	\+ NumPontes == 'X',
	length(Pontes, LenPontes),
	NumPontes == LenPontes,
	append(DepositoIlhas, [ilha(NumPontes,Posicao)], IlhasRecolhidas),
	ilhas_terminadas(OutrasEntradas, IlhasTerm, IlhasRecolhidas).

ilhas_terminadas(Estado, IlhasTerm, DepositoIlhas) :-					% se NumPontes == 'X', a ilha nao e considerada terminada
	Estado = [Entrada|OutrasEntradas],
	Entrada = [ilha(NumPontes,_), _, _],
	NumPontes == 'X',
	ilhas_terminadas(OutrasEntradas, IlhasTerm, DepositoIlhas), !.

ilhas_terminadas(Estado, IlhasTerm, DepositoIlhas) :-					% se o numero de pontes nao equivaler a NumPontes, a ilha nao e considerada terminada
	Estado = [Entrada|OutrasEntradas],
	Entrada = [ilha(NumPontes,_), _, Pontes],
	length(Pontes, LenPontes),
	\+ NumPontes == LenPontes,
	ilhas_terminadas(OutrasEntradas, IlhasTerm, DepositoIlhas), !.

ilhas_terminadas([], DepositoIlhas, DepositoIlhas).						% quando nao ha mais entradas a avaliar, IlhasTerm == DepositoIlhas





%	2.11-PREDICADO tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada),
% em que Ilhas_term e uma lista de ilhas terminadas e Entrada e uma entrada (ver
% Seccao 2.4), significa que Nova_entrada e a entrada resultante de remover as ilhas de
% Ilhas_term, da lista de ilhas vizinhas de entrada.





tira_ilhas_terminadas_entrada(Ilhas_Term, Entrada, Nova_Entrada) :-
	
	Entrada = [Ilha, Vizinhas, Pontes],
	Nova_Entrada = [Ilha, VizinhasAtuais, Pontes],
	findall(A, (member(A, Vizinhas), \+ member(A, Ilhas_Term)), VizinhasAtuais).





%	2.12-PREDICADO tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado), em que
% Estado e um estado (ver Seccao 2.4) e Ilhas_term e uma lista de ilhas terminadas,
% significa que Novo_estado e o estado resultante de aplicar o predicado
% tira_ilhas_terminadas_entrada a cada uma das entradas de Estado.





tira_ilhas_terminadas(Estado, IlhasTerm, NovoEstado):-
	Estado = [Entrada|OutrasEntradas],
	tira_ilhas_terminadas_entrada(IlhasTerm, Entrada, NovaEntrada),
	tira_ilhas_terminadas(OutrasEntradas, IlhasTerm, OutrasNovasEntradas),
	append([NovaEntrada], OutrasNovasEntradas, NovoEstado).

tira_ilhas_terminadas([], _, []).





%	2.13-PREDICADO marca_ilhas_terminadas_entrada(Ilhas_term, Entrada,
% Nova_entrada), em que Ilhas_term e uma lista de ilhas terminadas e Entrada
% e uma entrada (ver Seccao 2.4), significa que Nova_entrada e a entrada obtida de
% Entrada da seguinte forma: se a ilha de Entrada pertencer a Ilhas_term, o numero
% de pontes desta e substituido por 'X'; em caso contrario Nova_entrada e igual a
% Entrada.





marca_ilhas_terminadas_entrada(IlhasTerm, Entrada, NovaEntrada) :-		% se a ilha for terminal, substituir NumPontes por 'X'
	Entrada = [ilha(NumPontes,(L,C)), Vizinhas, Pontes],
	member(ilha(NumPontes,(L,C)), IlhasTerm),
	NovaEntrada = [ilha('X',(L,C)), Vizinhas, Pontes].

marca_ilhas_terminadas_entrada(IlhasTerm, Entrada, Entrada) :-			% se a ilha da entrada nao for terminal, NovaEntrada == Entrada
	Entrada = [ilha(NumPontes,(L,C)), _, _],
	\+ member(ilha(NumPontes,(L,C)), IlhasTerm).





%	2.14-PREDICADO marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado), em que
% Estado e um estado (ver Seccao 2.4) e Ilhas_term e uma lista de ilhas terminadas,
% significa que Novo_estado e o estado resultante de aplicar o predicado
% marca_ilhas_terminadas_entrada a cada uma das entradas de Estado.





marca_ilhas_terminadas(Estado, IlhasTerm, NovoEstado) :-
	Estado = [Entrada|OutrasEntradas],
	marca_ilhas_terminadas_entrada(IlhasTerm, Entrada, NovaEntrada),
	marca_ilhas_terminadas(OutrasEntradas, IlhasTerm, OutrasNovasEntradas),
	append([NovaEntrada], OutrasNovasEntradas, NovoEstado).

marca_ilhas_terminadas([], _, []).





%	2.15-PREDICADO trata_ilhas_terminadas(Estado, Novo_estado), em que Estado e um estado
% (ver Seccao 2.4), significa que Novo_estado e o estado resultante de aplicar
% os predicados tira_ilhas_terminadas e marca_ilhas_terminadas a Estado.





trata_ilhas_terminadas(Estado, NovoEstado):-
	ilhas_terminadas(Estado, IlhasTerm),
	tira_ilhas_terminadas(Estado, IlhasTerm, EstadoIntermedio),
	marca_ilhas_terminadas(EstadoIntermedio, IlhasTerm, NovoEstado).





%	2.16-PREDICADO junta_pontes(Estado, NumPontes, Ilha1, Ilha2, Novo_estado), em
% que Estado e um estado e Ilha1 e Ilha2 sao 2 ilhas, significa que Novo_estado e
% o estado que se obtem de Estado por adicao de NumPontes pontes entre Ilha1 e
% Ilha2.





junta_pontes(Estado, NumPontes, Ilha1, Ilha2, NovoEstado):-
	
	Ilha1 = ilha(_,Pos1),
	Ilha2 = ilha(_,Pos2),
	cria_ponte(Pos1,Pos2,Ponte),

	adicionar_ponte(Estado, Ponte, Ilha1, [], NumPontes, NovoEstado1),
	adicionar_ponte(NovoEstado1, Ponte, Ilha2, [], NumPontes, NovoEstado2),

	actualiza_vizinhas_apos_pontes(NovoEstado2, Pos1, Pos2, NovoEstado3),		% Actualiza o estado retirando as ilhas que ja nao sao vizinhas de cada entrada
	trata_ilhas_terminadas(NovoEstado3, NovoEstado).							% e retirando as ilhas finalizadas das vizinhas de cada entrada




adicionar_ponte(Entradas, Ponte, Ilha, DepositoEntradas, IndexPontes, EstadoAlterado) :- 	% se a entrada a analizar corresponde a ilha no 3o parametro

	Entradas = [Entrada|OutrasEntradas],
	Entrada = [Ilha, Vizinhas, Pontes],

	append(Pontes, [Ponte], PontesAlteradas),

	EntradaAlterada = [Ilha, Vizinhas, PontesAlteradas],

	append(DepositoEntradas, [EntradaAlterada], NovoDepositoEntradas),

	adicionar_ponte(OutrasEntradas, Ponte, Ilha, NovoDepositoEntradas, IndexPontes, EstadoAlterado).



adicionar_ponte(Entradas, Ponte, Ilha, DepositoEntradas, IndexPontes, EstadoAlterado) :- 	% se a entrada a analizar nao corresponde a ilha no 3o parametro

	Entradas = [Entrada|OutrasEntradas],
	Entrada = [Ilha2, _, _],
	\+ Ilha2 == Ilha,


	append(DepositoEntradas, [Entrada], NovoDepositoEntradas),

	adicionar_ponte(OutrasEntradas, Ponte, Ilha, NovoDepositoEntradas, IndexPontes, EstadoAlterado), !.		% o ! e utilizado para impedir que se gerem solucoes, nas quais o DepositoEntradas
																											% tenha variaveis anonimas

adicionar_ponte([], Ponte, Ilha, DepositoEntradas, IndexPontes, EstadoAlterado) :-			% para quando acaba uma passagem (em que se adicionou uma vez a ponte a entrada certa)
	
	\+ IndexPontes == 1,
	NovoIndexPontes is IndexPontes - 1,
	adicionar_ponte(DepositoEntradas, Ponte, Ilha, [], NovoIndexPontes, EstadoAlterado).

adicionar_ponte([], _, _, DepositoEntradas, 1, DepositoEntradas).

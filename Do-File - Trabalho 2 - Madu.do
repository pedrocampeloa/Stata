**************************************************************************************************************************************************
* TRABALHO 1 - LABORAT�RIO DE ECONOMETRIA - PROFESSORA MARIA EDUARDO TANNURI PIANTO
*ALUNOS: JOS� EDUARDO GON�ALVES DE SOUSA  -  MATR�CULA: 13/0011461
*        LENNON CARRIJO                   -  MAT�CULA: 12/0140179
**************************************************************************************************************************************************
clear
*COMANDOS PARA A REALIZA��O DOS PROCEDIMENTOS INICIAIS, O ESTADO SELECIONADO FOI O CEAR�*
* SELECIONANDO A PASTA DE TRABALHO:
cd "C:\Users\joseg_000\Desktop\Jos� Eduardo\Laborat�rio de Econometria\TRABALHO 2"


* A) EXTRA��O DA PNAD 2006:
infile using "pnad2006pes.dct", using ("C:\Users\joseg_000\Desktop\Jos� Eduardo\Laborat�rio de Econometria\TRABALHO 2\PES2006.txt")
use pnad2006pes.dct, clear
sort uf v0102 v0103
save pnad2006pes, replace

* B) UNINDO OS BANCOS DE PESSOAS E DOMIC�LIOS:
use pnad2006pes, clear
sort v0102 v0103
merge m:1 uf v0102 v0103 using pnad2006dom, generate (merge1)
destring uf, replace
keep if (uf==23) & merge1==3

* C) GERANDO DUMMY DE RECEBIMENTO DO BOLSA FAM�LIA:
gen bfamilia = v2403 if v2403~=9

* D) GERANDO DUMMY DE IDADE:
gen idade = v8005 if v8005~=999

* E) IDADE AO QUADRADO:
gen idade2 = idade^2

* F) ESCOLARIDADE:
gen escolaridade = v4703

* G) ESCOLARIDADE AO QUADRADO:
gen escolaridade2 = escolaridade^2

* H) VARI�VEL DUMMY DE SEXO:
gen sexo = v0302 if v0302~=4

* I) DUMMY DE COR:
gen ra�a = v0404 if v0404~=2~=6~=0~=9

* J) DUMMY DE FAM�LIA:
gen familia = v4723 if v4723<=5

* K) VARI�VEL "HORAS TRABALHADAS":
egen horas_trabalho = rowtotal(v9058 v9101 v9105) 
recode horas_trabalho 0=.

* L) DUMMY DE PARTICIPA��O NO MERCADO DE TRABALHO:
gen participacao = (v9001==1) if v9001~=.
recode participacao 1=0 if horas_trabalho==.
recode participacao 0=1 if horas_trabalho~=.

* M) DUMMY DE �REA DE RESID�NCIA RURAL:
gen rural = v4728>=4

* N) VARI�VEL DE RENDA DOMICILIAR PER-CAPTA N�O PROVENIENTE DO TRBALHO, EXCLUINDO A RENDA DO CHEFE DO DOMIC�LIO:
recode v4721 999999999999=.
recode v4719 999999999999=.
gen menos_v4719= -1*v4719
egen renda_dom_lchefe=rowtotal(v4721 menos_v4719)
gen renda_lchefepc=renda_dom_lchefe/v4741

* O) VARI�VEL REFERENTE AO NUMERO DE FILHOS MAIORES OU MENORES QUE 14 ANOS DE IDADE:
***************n�mero de filhos menores de 14 anos**************
gen condicao_filhos_l14 = (v0402==3 & v8005<=14)
bys uf v0102 v0103: 
egen filhos_l14 = total(condi��o_filhos_l14)
***********n�mero de filhos maiores de 14 anos*****************
gen condicao_filhos_m14 = (v0402==3 & v0805>14)
bys uf v0102 v0103:
egen filhos_m14 = total(condicao_filhos_m14)

* P) MANTER SOMENTE OBSERVA��ES SOBRE CHEFES DE DOMIC�LIO:
gen chefe = v0401 if v0401==1

*********************************************************************************************************************************************************************************

* FINALIZADOS OS AJUSTES INICIAIS, PASSAMOS AOS EXERC�CIOS:

*********************** QUEST�O 1 *************************

*GERANDO A SEMENTE ALEAT�RIA*
gen cod = _n
set seed 1000
gen x = (1)
sort x

*USANDO PSM*
ssc install psmatch2, replace
g att = .
psmatch2 att[idade idade2 escolaridade escolaridade2 sexo ra�a familia rural renda_lchefepc] [if exp] [in range] , [outcome(treat)
                     pscore(att) neighbor(integer k>1) caliper(real)
                     common trim(real) odds index logit ties nowarnings
                     quietly ate]

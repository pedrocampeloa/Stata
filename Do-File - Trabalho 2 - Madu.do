**************************************************************************************************************************************************
* TRABALHO 1 - LABORATÓRIO DE ECONOMETRIA - PROFESSORA MARIA EDUARDO TANNURI PIANTO
*ALUNOS: JOSÉ EDUARDO GONÇALVES DE SOUSA  -  MATRÍCULA: 13/0011461
*        LENNON CARRIJO                   -  MATÍCULA: 12/0140179
**************************************************************************************************************************************************
clear
*COMANDOS PARA A REALIZAÇÃO DOS PROCEDIMENTOS INICIAIS, O ESTADO SELECIONADO FOI O CEARÁ*
* SELECIONANDO A PASTA DE TRABALHO:
cd "C:\Users\joseg_000\Desktop\José Eduardo\Laboratório de Econometria\TRABALHO 2"


* A) EXTRAÇÃO DA PNAD 2006:
infile using "pnad2006pes.dct", using ("C:\Users\joseg_000\Desktop\José Eduardo\Laboratório de Econometria\TRABALHO 2\PES2006.txt")
use pnad2006pes.dct, clear
sort uf v0102 v0103
save pnad2006pes, replace

* B) UNINDO OS BANCOS DE PESSOAS E DOMICÍLIOS:
use pnad2006pes, clear
sort v0102 v0103
merge m:1 uf v0102 v0103 using pnad2006dom, generate (merge1)
destring uf, replace
keep if (uf==23) & merge1==3

* C) GERANDO DUMMY DE RECEBIMENTO DO BOLSA FAMÍLIA:
gen bfamilia = v2403 if v2403~=9

* D) GERANDO DUMMY DE IDADE:
gen idade = v8005 if v8005~=999

* E) IDADE AO QUADRADO:
gen idade2 = idade^2

* F) ESCOLARIDADE:
gen escolaridade = v4703

* G) ESCOLARIDADE AO QUADRADO:
gen escolaridade2 = escolaridade^2

* H) VARIÁVEL DUMMY DE SEXO:
gen sexo = v0302 if v0302~=4

* I) DUMMY DE COR:
gen raça = v0404 if v0404~=2~=6~=0~=9

* J) DUMMY DE FAMÍLIA:
gen familia = v4723 if v4723<=5

* K) VARIÁVEL "HORAS TRABALHADAS":
egen horas_trabalho = rowtotal(v9058 v9101 v9105) 
recode horas_trabalho 0=.

* L) DUMMY DE PARTICIPAÇÃO NO MERCADO DE TRABALHO:
gen participacao = (v9001==1) if v9001~=.
recode participacao 1=0 if horas_trabalho==.
recode participacao 0=1 if horas_trabalho~=.

* M) DUMMY DE ÁREA DE RESIDÊNCIA RURAL:
gen rural = v4728>=4

* N) VARIÁVEL DE RENDA DOMICILIAR PER-CAPTA NÃO PROVENIENTE DO TRBALHO, EXCLUINDO A RENDA DO CHEFE DO DOMICÍLIO:
recode v4721 999999999999=.
recode v4719 999999999999=.
gen menos_v4719= -1*v4719
egen renda_dom_lchefe=rowtotal(v4721 menos_v4719)
gen renda_lchefepc=renda_dom_lchefe/v4741

* O) VARIÁVEL REFERENTE AO NUMERO DE FILHOS MAIORES OU MENORES QUE 14 ANOS DE IDADE:
***************número de filhos menores de 14 anos**************
gen condicao_filhos_l14 = (v0402==3 & v8005<=14)
bys uf v0102 v0103: 
egen filhos_l14 = total(condição_filhos_l14)
***********número de filhos maiores de 14 anos*****************
gen condicao_filhos_m14 = (v0402==3 & v0805>14)
bys uf v0102 v0103:
egen filhos_m14 = total(condicao_filhos_m14)

* P) MANTER SOMENTE OBSERVAÇÕES SOBRE CHEFES DE DOMICÍLIO:
gen chefe = v0401 if v0401==1

*********************************************************************************************************************************************************************************

* FINALIZADOS OS AJUSTES INICIAIS, PASSAMOS AOS EXERCÍCIOS:

*********************** QUESTÃO 1 *************************

*GERANDO A SEMENTE ALEATÓRIA*
gen cod = _n
set seed 1000
gen x = (1)
sort x

*USANDO PSM*
ssc install psmatch2, replace
g att = .
psmatch2 att[idade idade2 escolaridade escolaridade2 sexo raça familia rural renda_lchefepc] [if exp] [in range] , [outcome(treat)
                     pscore(att) neighbor(integer k>1) caliper(real)
                     common trim(real) odds index logit ties nowarnings
                     quietly ate]

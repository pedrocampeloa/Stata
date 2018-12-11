*TRABALHO 2 - LABORATÓRIO DE ECONOMETRIA 
*Yan Moreira do Rêgo Barros – 12/0138671
*Pedro Campelo 12/0153483


cd "C:\Users\Yan\Desktop\Lab eco\Stata12\trabalho 2\dados"
cd "C:\Users\Yan\Desktop\Lab eco\Stata12\trabalho 2\dicionário"
infile using pnad2006dom.dct, using ("C:\Users\Yan\Desktop\Lab eco\Stata12\trabalho 2\dados")
cd "C:\Users\Yan\Desktop\Lab eco\Stata12\trabalho 2"
infile using pnad2006dom.dct, using ("C:\Users\Yan\Desktop\Lab eco\Stata12\trabalho 2\dados")
cd "C:\Users\Yan\Desktop\Lab eco\Stata12\trabalho 2"
infile using pnad2006dom.dct, using ("C:\Users\Yan\Desktop\Lab eco\Stata12\trabalho 2\dados\DOM2006.txt)
infile using pnad2006dom.dct, using ("C:\Users\Yan\Desktop\Lab eco\Stata12\trabalho 2\dados\DOM2006.txt")
save "C:\Users\Yan\Desktop\Lab eco\Stata12\trabalho 2\dom2006b.dta"
clear
infile using pnad2006pes.dct, using ("C:\Users\Yan\Desktop\Lab eco\Stata12\trabalho 2\dados\PES2006.txt")
save "C:\Users\Yan\Desktop\Lab eco\Stata12\trabalho 2\pes2006b.dta"
clear
use "C:\Users\Yan\Desktop\Lab eco\Stata12\trabalho 2\pes2006b.dta", clear*Transformar variáveis texto para numéricas no arquivo de pessoas

destring, replace
 sort  v0102 v0103
save PES2006sorted, replace
sort uf  v0102 v0103
save PES2006sorted, replace
clear
use "C:\Users\Yan\Desktop\Lab eco\Stata12\trabalho 2\dom2006b.dta", clear
*Transformar variáveis texto para numéricas no arquivo de domicílios

destring, replace
sort uf  v0102 v0102
save DOM2006sorted, replace
clear
use "C:\Users\Yan\Desktop\Lab eco\Stata12\trabalho 2\PES2006sorted.dta", clear

*Juntar as duas bases de dados, ordenando as variaveis
merge m:1  uf v0102 v0103 using DOM2006SORTED, generate(merge1)
destring, replace
save PESDOM2006merged, replace
keep if uf==23 & merge==3
save PESDOM2006mergedCeara
browse v2403
drop if v2403==9
*Criar a Dummy para quem recebeu bolsa familia

gen BolsaFamilia = 1 if v2403==1
browse bolsafamilia
browse BolsaFamilia
replace BolsaFamilia = 0 if v2403==3
browse v2403
browse BolsaFamilia
tab v2403
*Criar variável para idade

gen idade = v8005
drop if idade >=120
tab idade
*Criar variável para idade

gen idade2 = idade^2
*Criar variável para escolaridade

gen anosestudo = v4703 - 1
drop if anosestudo ==16
gen anosestudo2 = anosestudo^2
tab anoestudo
tab anosestudo
*Criar dummy de sexo

gen masculino = 0
replace masculino =1 if v0302==2
*Criar dummy de cor

gen preto_pardo = 0
replace preto_pardo= 1 if v0404 == 4 | v0404 == 8
tab preto_pardo
*Criar dummy para a familia que é composta por um casal
gen casal = 0
replace casal = 1 if v4723==1| v4723 == 02 | v4723 == 03 | v4723 == 04 | v4723 == 05

*Criar variável para hora de trabalho, somando todos os trabalhos

egen horas_trabalho = rowtotal(v9058 v9101 v9105) recode horas_trabalho 0=.
egen horas_trabalho = rowtotal(v9058 v9101 v9105) recode horas_trabalho 0=.
egen horas_trabalho = rowtotal(v9058 v9101 v9105)
recode horas_trabalho 0=.
browse v9058 v9101 v9105 horas_trabalho idade

*Criar ln de horas_trabalho

gen lnhoras_trabalho = ln(horas_trabalho)
*Criar variável para participação no mercadod de trabalho

gen participacao = (V9001==1) if V9001~=.
recode participacao 1=0 if horas_trabalho==.
recode participacao 0=1 if horas_trabalho~=.
gen participacao = (v9001==1) if v9001~=.
recode participacao 1=0 if horas_trabalho==.
recode participacao 0=1 if horas_trabalho~=.
tab participacao
*Criar variável para pessoas que moram em área rual

gen rural=(V4728>=4)
gen rural=(v4728>=4)

*Criar variável para frequencia escolar 
gen freq_escola = (v0603==1 & v0605>=1)
tab freq_escola
*Criar variável referente a renda domiciliar per capita não proveninenet do trabalho, excluindo a renda do chege do domicílio 
recode v4721 999999999999=.
recode v4719 999999999999=.
gen menos_v4719= -1*v4719
egen renda_dom_lchefe=rowtotal(v4721 menos_v4719
gen renda_lchefepc=renda_dom_lchefe/v4741
egen renda_dom_lchefe=rowtotal(v4721 menos_v4719)

gen renda_lchefepc=renda_dom_lchefe/v4741

*Criar variável para filhos com menos de 14 anos
gen condicao_filhos_l14=(v0402==3 & v8005<=14)
bys uf v0102 v0103: egen filhos_l14=total(condicao_filhos_l14)

*Criar variável para filhos com mais de 14 anos
gen condicao_filhos_mais14=(v0402==3 & v8005>14)
bys uf v0102 v0103: egen filhos_mais14=total(condicao_filhos_mais14)
*Criar variável para chefe de família

gen chefefamilia = 0
replace chefefamilia = 1 if v0401 == 1
save PESDOM2006intro, replace

***************** QUESTÃO 1 *******************

*Regredir oferta de trabalho em funcao das pessoas que recebem BF: análise de que o bolsa familia diminui a oferta de trabalho

reg horas_trabalho BolsaFamilia idade idade2 anosestudo anosestudo2 masculino preto_pardo casal rural renda_lchefepc

*Verificar a média de cada variável para as pessoas que recebem e as que não recebem o bolsa familia

mean idade idade2 anosestudo anosestudo2 masculino preto_pardo casal rural renda_lchefepc, over (BolsaFamilia)

sum idade idade2 anosestudo anosestudo2 masculino preto_pardo casal rural renda_lchefepc if BolsaFamilia==0

sum idade idade2 anosestudo anosestudo2 masculino preto_pardo casal rural renda_lchefepc if BolsaFamilia==1

*Testas a hispotese dos coeficisntes das variáveis acima serem iguais a zero

test ([idade]_b[1] = [idade]_b[0]) ([idade2]_b[1] = [idade2]_b[0]) ([anosestudo]_b[1] = [anosestudo]_b[0]) ([anosestudo2]_b[1] = [anosestudo2]_b[0]) ([masculino]_b[1] = [masculino]_b[0]) ([preto_pardo]_b[1] = [preto_pardo]_b[0]) ([casal]_b[1] = [casal]_b[0]) ([rural]_b[1] = [rural]_b[0]) ([renda_lchefepc]_b[1] = [renda_lchefepc]_b[0])

*GERANDO A SEMENTE ALEATÓRIA*
gen cod = _n
set seed 1000
gen x = (1)
sort x

*Estimando por LOGIT
. logit  BolsaFamilia idade idade2 anosestudo anosestudo2 masculino preto_pardo casal rural

*Estimando pelo PSCORE

pscore BolsaFamilia rural masculino preto_pardo, pscore(pscore1) comsup numblo (5) level (0.005) logit

attk horas_trabalho BolsaFamilia rural masculino preto_pardo, pscore(pscore1) comsup numblo (5)

*Estimando pelo PSMATCH2

psmatch2 BolsaFamilia, kernel outcome(horas_trabalho) kerneltype(normal) pscore(pscore1) common odds index logit quietly ate

psmatch2 BolsaFamilia, n(5) outcome(horas_trabalho) pscore(pscore1) common odds index logit quietly ate

*Fazer gráfico de estimativa de densidade de Kerner (mostrar que a concentração de horas de trabalho é maior para as pessoas que nao recebem BF)

twoway kdensity lnhoras_trabalho if BolsaFamilia==1 || kdensity lnhoras_trabalho if BolsaFamilia==0 || , legend(label(1 treated) label(2 control))


*Teste de KORMOV SMIRNOV para verificar a distribuição normal

ksmirnov horas_trabalho, by (BolsaFamilia)




***************** QUESTÃO 2 *******************

use "C:\Users\Yan\Desktop\Lab eco\Stata12\trabalho 2\PESDOM2006intro.dta", clear

gen h =  renda_lchefepc>=139 &  renda_lchefepc<=159

g xcl =   renda_lchefepc - 149 if h==1 &  renda_lchefepc<=149

g xcr =  renda_lchefepc - 149 if h==1 &  renda_lchefepc>149

reg  freq_escola xcl if xcl~=. [aw=v4729]

loc alfayl = _b[_cons]

reg  freq_escola xcr if xcr~=. [aw=v4729]

loc alfayr = _b[_cons]

recode v2403 (3=0), copyrest g(treat)

reg treat xcl if xcl~=. [aw=v4729]

loc alfatr = _b[_cons]

di (`alfayl' - `alfayr')/ (`alfatl' - `alfatr')

g instru =  renda_lchefepc>=139 &   renda_lchefepc<=149

gen xcli = cond( renda_lchefepc>=139 &  renda_lchefepc<=149, renda_lchefepc - 149,0)

gen xcri = cond( renda_lchefepc>=150 &  renda_lchefepc<=159, renda_lchefepc - 149,0)

ivregress 2sls  freq_escola xcli xcri (treat = instru) if h==1 [aw=v4729]

reg  horas_trabalho xcl if xcl~=. [aw=v4729]

ivregress 2sls  horas_trabalho xcli xcri (treat = instru) if h==1 [aw=v4729]

clear

use "C:\Users\Yan\Desktop\Lab eco\Stata12\trabalho 2\PESDOM2006intro.dta", clear

gen h =  renda_lchefepc>=129 &  renda_lchefepc<=169

g xcl =   renda_lchefepc - 149 if h==1 &  renda_lchefepc<=149

g xcr =  renda_lchefepc - 149 if h==1 &  renda_lchefepc>149

recode v2403 (3=0), copyrest g(treat)

g instru =  renda_lchefepc>=129 &   renda_lchefepc<=149

gen xcli = cond( renda_lchefepc>=129 &  renda_lchefepc<=149, renda_lchefepc - 149,0)

gen xcri = cond( renda_lchefepc>=150 &  renda_lchefepc<=169, renda_lchefepc - 149,0)

ivregress 2sls  freq_escola xcli xcri (treat = instru) if h==1 [aw=v4729]

ivregress 2sls  horas_trabalho xcli xcri (treat = instru) if h==1 [aw=v4729]

clear

use "C:\Users\Yan\Desktop\Lab eco\Stata12\trabalho 2\PESDOM2006intro.dta", clear

gen h =  renda_lchefepc>=144 &  renda_lchefepc<=153

clear

use "C:\Users\Yan\Desktop\Lab eco\Stata12\trabalho 2\PESDOM2006intro.dta", clear

gen h =  renda_lchefepc>=144 &  renda_lchefepc<=154

g xcl =   renda_lchefepc - 149 if h==1 &  renda_lchefepc<=149

g xcr =  renda_lchefepc - 149 if h==1 &  renda_lchefepc>149

recode v2403 (3=0), copyrest g(treat)

g instru =  renda_lchefepc>=129 &   renda_lchefepc<=149

gen xcli = cond( renda_lchefepc>=144 &  renda_lchefepc<=149, renda_lchefepc - 149,0)

gen xcri = cond( renda_lchefepc>=150 &  renda_lchefepc<=154, renda_lchefepc - 149,0)

ivregress 2sls  freq_escola xcli xcri (treat = instru) if h==1 [aw=v4729]

ivregress 2sls  horas_trabalho xcli xcri (treat = instru) if h==1 [aw=v4729]


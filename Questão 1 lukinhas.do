cd "C:\Users\Naves\Desktop\Lab. de Econometria\Trabalho 2"
infile using "pnad2006dom.dct", using ("C:\Users\Naves\Desktop\Lab. de Econometria\Trabalho 2\dados\DOM2006.txt")
save "C:\Users\Naves\Desktop\Lab. de Econometria\Trabalho 2\DOM.dta"
infile using "pnad2006dom.dct", using ("C:\Users\Naves\Desktop\Lab. de Econometria\Trabalho 2\dados\DOM2006.txt")
destring, replace
sort uf V0102 V0103
destring, replace
use PES1.dta, clearmerge m:1  uf v0102 v0103 using DOM1.dta, generate(merge1)
destring uf, replace
keep if uf==29 & merge1==3
save MERGE, replace
gen recebeuBF = 0
drop if v2403 >=9
replace recebeuBF=0 if v2403==3
replace recebeuBF=1 if v2403==1
drop if v8005==999
gen idade = v8005
gen idade2 = idade^2
gen escolaridade = v4703-1
drop if escolaridade==16
gen escolaridade2 = escolaridade^2
gen masculino = 0
replace masculino = 1 if v0302==2
gen pretaouparda = 0
replace pretaouparda = 1 if v0404==4|v0404==8
gen casal = 0
replace casal = 1 if v4723==1|v4723==2|v4723==3|v4723==4|v4723==5
egen horas_trabalho = rowtotal(v9058 v9101 v9105)
recode horas_trabalho 0=.
gen participacao = (v9001==1) if v9001~=.
recode participacao 1=0 if horas_trabalho==.
recode participacao 0=1 if horas_trabalho~=.
gen rural=(v4728>=4)
recode V4721 999999999999=.
recode V4719 999999999999=.
recode v4721 999999999999=.
recode v4719 999999999999=.
gen menos_v4719= -1*v4719

egen renda_dom_lchefe= rowtotal(v4721 menos_v4719)
gen renda_lchefepc=renda_dom_lchefe/v4741
gen condicao_filhos_l14=(v0402==3 & v8005<=14)

bys uf v0102 v0103: egen filhos_l14=total(condição_filhos_114)

gen condicao_filhos_m14=(v0402==3 & v8005>14)

bys uf v0102 v0103: egen filhos_m14=total(condicao_filhos_m14)

keep if v0401==1

gen freq_escola=(v0603==1 & v0605>=1)

mean idade idade2 escolaridade escolaridade2 masculino pretaouparda casal rural renda_lchefepc, over (recebeuBF)

test ([idade]_b[1] = [idade]_b[0]) ([idade2]_b[1] = [idade2]_b[0]) ([escolaridade]_b[1] = [escolaridade]_b[0]) ([escolaridade2]_b[1] = [escolaridade2]_b[0]) ([masculino]_b[1] = [masculino]_b[0]) ([pretaouparda]_b[1] = [pretaouparda]_b[0]) ([casal]_b[1] = [casal]_b[0]) ([rural]_b[1] = [rural]_b[0]) ([renda_lchefepc]_b[1] = [renda_lchefepc]_b[0])

regress horas_trabalho recebeuBF

generate lhoras_trabalho = ln(horas_trabalho)

twoway kdensity lhoras_trabalho if recebeuBF==1 || kdensity lhoras_trabalho if recebeuBF==0 || , legend(label(1 treated) label(2 control))

ksmirnov horas_trabalho, by (recebeuBF)

pscore recebeuBF escolaridade escolaridade2 masculino pretaouparda, pscore(pscore1) comsup numblo (5) level (0.005) logit

psmatch2 recebeuBF, kernel outcome(horas_trabalho) kerneltype(normal) pscore(pscore1) common odds index logit quietly ate

psmatch2 recebeuBF, n(5) outcome(horas_trabalho) pscore(pscore1) common odds index logit quietly ate

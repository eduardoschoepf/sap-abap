<img src="/assets/sap_logo.png" alt="SAP logo" width="100"/>  

Este guia abrange a história, evolução, módulos, conceitos e ferramentas SAP. Use os links abaixo para explorar cada seção:

## Histórico da SAP
- [Fundação e Evolução](fundacao_e_evolucao.md)

## Módulos SAP
- [Sales and Distribution (SD)](modulo_sd.md)
- [Controlling (CO)](modulo_co.md)
- [Materials Management (MM)](modulo_mm.md)
- [Financial Accounting (FI)](modulo_fi.md)
- [Production Planning (PP)](modulo_pp.md)

---

## SAP ABAP
- [ABAP (Advanced Business Application Programming)](linguagem_abap.md)
- [Criar um package](package.md)
- [Criar um programa](#criar-um-programa)
- [Atalhos úteis no ambiente ABAP](#atalhos-úteis-no-ambiente-abap)
- [Modelo de cabeçalho](#modelo-de-cabeçalho)


### Criar um programa
Selecionar o package **(transação SE80)** e com um clique direito selecinar **CREATE -> PROGRAM**. Inserir o nome do programa, e clicar em Create.
Na janela de propriedades, devemos definir o **título** e o **tipo (Programa executável)**.  
Clique em Save para salvar as alterações.


### Atalhos úteis no ambiente ABAP
**Ctr+S**: Salva as alterações no programa (o programa passa para o estado inativo).  
**Ctr+F2**: Verifica a sintaxe do código.  
**Ctr+F3**: Ativa o programa, tornando-o executável.  
**F8**: Executa o programa.  

### Modelo de cabeçalho
```
*=====================================================================*
* Report......: Z_PROG                                                *
* Author......: ESCHOEPF                                              *
* Date........: --.--.---- - --:--                                    *
*---------------------------------------------------------------------*
* Description.:                                                       *
*---------------------------------------------------------------------*
* Date         Author       Description                               *
* --.--.----   --------     -----------------                         *
*=====================================================================*
```

# ABAP: Comandos de Declaração de Dados

## DATA
Declara uma variável ou tabela interna. É usado para criar variáveis que armazenam dados temporários no programa.  

**Declaração:**  
```abap
DATA: nome TYPE string.
```

**Declaração de Estruturas:**  
```abap
TYPES: BEGIN OF ls_struc,
         nome TYPE string,
         idade TYPE i,
       END OF ls_struc.
DATA: it_stru1 TYPE SORTED TABLE OF ls_struc WITH UNIQUE KEY nome.
```

### Dicionário  

**BAPI (Business Application Programming Interface)** é uma interface de programação que permite a integração entre sistemas SAP e outras aplicações externas. As BAPIs são métodos que pertencem a objetos de negócios no SAP e são usados para realizar operações específicas, como criar, ler, atualizar ou deletar dados no sistema SAP.  

**BADI (Business Add-In)** é uma técnica de extensão que permite personalizar e adaptar funcionalidades padrão do sistema SAP sem a necessidade de modificar o código-fonte original. As BADIs são parte do conceito de programação orientada a objetos no SAP e são projetadas para ser implementadas de forma independente das atualizações do sistema, o que ajuda a preservar as personalizações durante upgrades.

**In-App Extensibility** permite que os usuários ajustem o comportamento e a interface do sistema SAP sem modificar o código-fonte subjacente. Isso significa que as customizações são realizadas em uma camada superior, o que ajuda a garantir que essas alterações sejam preservadas durante atualizações e upgrades do sistema.

**Enhancement Points** são locais pré-definidos no código padrão do SAP onde é possível adicionar customizações sem modificar o código original.

**BASIS** Business Application Software Integrated Solution.

**DEV** Development  
**QAS** Quality Assurance  
**PROD** Production  

**SWPM (Software Provisioning Manager)** é uma ferramenta utilizada pelo consultor BASIS para instalar, desinstalar e realizar outras tarefas de administração e manutenção em sistemas SAP, como a migração e cópia de sistemas. É uma evolução do antigo "SAPInst" e oferece uma interface simplificada e guiada para facilitar essas operações em diferentes ambientes e plataformas SAP.  

**SUM (Software Update Manager)** é uma ferramenta usada para gerenciar e aplicar atualizações, patches, upgrades e conversões em sistemas SAP. Ele é crucial para manter o ambiente SAP atualizado e seguro, permitindo a aplicação de melhorias e correções de forma eficiente, minimizando o tempo de inatividade e o impacto nas operações.

**PI (Process Integration)** é uma ferramenta de middleware que permite a integração de diferentes sistemas SAP e não-SAP dentro de uma organização.  

**PO (Process Orchestration)** é uma evolução do SAP PI que combina funcionalidades de integração (SAP PI), gestão de processos de negócios (BPM - Business Process Management) e regras de negócios (BRM - Business Rules Management).

**CPI (Cloud Platform Integration)** é a solução de integração baseada na nuvem, parte do SAP Business Technology Platform (BTP).

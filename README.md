<img src="/assets/sap_logo.png" alt="SAP logo" width="100"/>  

# Guia SAP
Este guia abrange a história, evolução, módulos, conceitos e ferramentas SAP. Use os links abaixo para explorar cada seção:

## Histórico da SAP
- [Fundação e Evolução](fundacao_e_evolucao.md)

## Módulos SAP
- [Sales and Distribution (SD)](modulo_sd.md)
- [Controlling (CO)](modulo_co.md)
- [Materials Management (MM)](modulo_mm.md)
- [Financial Accounting (FI)](modulo_fi.md)
- [Production Planning (PP)](modulo_pp.md)

  
# Resumo dos Módulos SAP

## Módulo SD (Sales and Distribution)
O módulo **Sales and Distribution (SD)** é responsável pela gestão das vendas e distribuição de produtos. As principais funções incluem:
- **Processamento de Pedidos**: Gerenciar e processar pedidos de clientes.
- **Gestão de Vendas**: Controlar o ciclo de vendas, desde a oferta até a faturação.
- **Distribuição e Logística**: Coordenar o envio e a entrega de produtos aos clientes.
- **Relatórios de Vendas**: Gerar relatórios sobre o desempenho de vendas e análise de mercado.

## Módulo CO (Controlling)
O módulo **Controlling (CO)** do SAP é responsável por ajudar as empresas a gerenciar e controlar seus processos internos. Ele fornece ferramentas para:
- **Custos e Despesas**: Monitorar e controlar custos e despesas.
- **Análise de Lucro**: Avaliar a rentabilidade por centro de custo, centro de lucro, e outros critérios.
- **Planejamento**: Facilitar o planejamento e orçamento financeiro.
- **Relatórios**: Gerar relatórios de controle financeiro e de desempenho.

## Módulo MM (Materials Management)
O módulo **Materials Management (MM)** abrange todos os aspectos do gerenciamento de materiais e estoque. Suas principais funções são:
- **Gestão de Inventário**: Controlar e otimizar o estoque de materiais.
- **Compras**: Gerenciar pedidos de compra, fornecedores e contratos.
- **Recebimento e Armazenagem**: Processar a entrada de materiais e seu armazenamento.
- **Avaliação de Fornecedores**: Avaliar o desempenho dos fornecedores e a qualidade dos materiais.

## Módulo FI (Financial Accounting)
O módulo **Financial Accounting (FI)** é essencial para o gerenciamento das finanças de uma empresa. Ele inclui:
- **Contabilidade Geral**: Manter registros financeiros precisos e completos.
- **Contas a Pagar e Receber**: Gerenciar pagamentos e recebimentos de clientes e fornecedores.
- **Livro Razão**: Controlar e consolidar todas as transações financeiras.
- **Relatórios Financeiros**: Gerar balanços, demonstrações de resultados e outros relatórios financeiros.

## Módulo PP (Production Planning)
O módulo **Production Planning (PP)** lida com todos os aspectos da produção e planejamento de manufatura. Ele inclui:
- **Planejamento de Produção**: Definir e programar a produção com base na demanda.
- **Controle de Produção**: Monitorar o progresso da produção e ajustar conforme necessário.
- **Gestão de Materiais**: Garantir a disponibilidade de materiais para a produção.
- **Custos de Produção**: Calcular e controlar os custos associados à fabricação.
---

# SAP ABAP

ABAP (Advanced Business Application Programming) é uma linguagem de programação de alto nível desenvolvida pela empresa de software SAP. É a principal linguagem utilizada no produto mais conhecido desta empresa, o SAP R/3, um software ERP. O ABAP tem uma sintaxe semelhante ao COBOL.

## Formação ABAP

1. [Package](#package)
2. [Criar um package](#criar-um-package)
3. [Criar um programa](#criar-um-programa)
4. [Atalhos úteis no ambiente ABAP](#atalhos-úteis-no-ambiente-abap)
5. [Modelo de cabeçalho](#modelo-de-cabeçalho)

### Package
Um PACKAGE é uma estrutura hierárquica usada para agrupar e organizar diferentes objetos. E possivel definir uma estrutura lógica para os programas e classes ABAP.  

### Criar um package
Acessar o ambiente de desenvolvimento SAPGUI **(transação SE80)** e abrir o editor de gerenciamento de packages **(transação SE21)**. Clicar em "Create" para criar um novo package.
Nomear o package usando uma convenção de nomenclatura clara, consistente e que respeite as convenções do cliente.  
Definir a visibilidade do package: público, privado ou protegido.  
Depois que o pacote for criado, podemos usá-lo para organizar objetos de desenvolvimento, como programas, classes, funções etc.

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

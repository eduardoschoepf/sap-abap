# SAP

## 1972 - Criação do SAP
- **1972**: Fundação da SAP (Systemanalyse und Programmentwicklung) por cinco ex-funcionários da IBM na Alemanha.
- **1973**: Lançamento do primeiro produto SAP R/1, um sistema de contabilidade.

## 1990 - SAP R/3
- **1992**: Lançamento do SAP R/3, um sistema de ERP com arquitetura cliente-servidor, marcando a primeira grande evolução do produto e expansão para diversas áreas além da contabilidade.
- **1996**: Introdução do SAP R/3 Release 3.1, com novas funcionalidades e melhorias na integração.

## 2000 - Expansão e Inovações
- **2000**: Lançamento do SAP R/3 Enterprise, com novas capacidades e melhorias para suportar empresas em crescimento.
- **2001**: Introdução do SAP NetWeaver, uma plataforma de integração e desenvolvimento que facilita a integração de diferentes sistemas.

## 2004 - SAP ERP e Business Suite
- **2004**: Lançamento do SAP ERP (Enterprise Resource Planning), consolidando as soluções em um único pacote.
- **2005**: Introdução do SAP Business Suite, que inclui SAP ERP, SAP CRM, SAP SRM e SAP PLM.

## 2007 - SAP Business ByDesign
- **2007**: Lançamento do SAP Business ByDesign, uma solução ERP baseada em nuvem para pequenas e médias empresas.

## 2010 - SAP HANA
- **2010**: Lançamento do SAP HANA (High-Performance Analytic Appliance), uma plataforma de banco de dados in-memory que revolucionou o processamento e a análise de dados em tempo real.
- **2011**: Introdução do SAP HANA como uma plataforma para aplicações de análise e processamento transacional.

## 2013 - SAPUI5
- **2013**: Lançamento do SAPUI5, um toolkit de desenvolvimento de interface de usuário para construir aplicações web responsivas e modernas. Baseado em HTML5 e JavaScript, o SAPUI5 é usado para criar interfaces ricas e adaptáveis.

## 2015 - SAP Fiori
- **2015**: Introdução do SAP Fiori, um conjunto de diretrizes de design e uma coleção de aplicativos que proporcionam uma experiência de usuário mais intuitiva e consistente. O Fiori é construído com SAPUI5 e é projetado para melhorar a usabilidade e a eficiência das aplicações SAP.

## 2015 - SAP S/4HANA
- **2015**: Lançamento do SAP S/4HANA, uma nova geração de soluções de ERP baseada na plataforma SAP HANA, com uma interface de usuário moderna e simplificada.

## 2017 - Expansão da Nuvem
- **2017**: Lançamento do SAP Cloud Platform, uma plataforma de desenvolvimento em nuvem que permite a criação de aplicativos empresariais e extensões para soluções SAP.

## 2020 - SAP Business Technology Platform
- **2020**: Rebranding da SAP Cloud Platform para SAP Business Technology Platform (BTP), incorporando recursos de integração, análise e desenvolvimento.

## 2021 - SAP Business Network
- **2021**: Lançamento do SAP Business Network, uma rede de colaboração empresarial para conectar empresas e otimizar a cadeia de suprimentos.

## 2023 - Avanços Contínuos
- **2023**: Continuação da evolução das soluções SAP, com melhorias em SAP S/4HANA, SAP BTP, e novos recursos para apoiar a transformação digital e a inovação empresarial.

---

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

---

# SAP_ABAP

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

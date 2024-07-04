# SAP_ABAP
## Formação ABAP   
Um PACKAGE é uma estrutura hierárquica usada para agrupar e organizar diferentes objetos. E possivel definir uma estrutura lógica para os programas e classes ABAP.  

### Criar um package :  
Acessar o ambiente de desenvolvimento SAPGUI **(transação SE80)** e abrir o editor de gerenciamento de packages **(transação SE21)**. Clicar em "Create" para criar um novo package.
Nomear o package usando uma convenção de nomenclatura clara, consistente e que respeite as convenções do cliente.  
Definir a visibilidade do package: público, privado ou protegido.  
Depois que o pacote for criado, podemos usá-lo para organizar objetos de desenvolvimento, como programas, classes, funções etc.

### Criar um programa :  
Selecionar o package **(transação SE80)** e com um clique direito selecinar **CREATE -> PROGRAM**. Inserir o nome do programa, e clicar em Create.
Na janela de propriedades, devemos definir o **título** e o **tipo (Programa executável)**.  
Clique em Save para salvar as alterações.


### Atalhos úteis no ambiente ABAP:  
**Ctr+S**: Salva as alterações no programa (o programa passa para o estado inativo).  
**Ctr+F2**: Verifica a sintaxe do código.  
**Ctr+F3**: Ativa o programa, tornando-o executável.  
**F8**: Executa o programa.  

### Header template
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

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

[⬅️ Voltar ao Sumário Principal](README.md)

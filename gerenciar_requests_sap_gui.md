# 📦 Como Gerenciar Requests no SAP GUI

---

## 🛠️ Criar uma Request

1. **Inicie a modificação de um objeto** (ex: programa, tabela, dicionário de dados).
2. Ao salvar ou ativar, o SAP solicitará a atribuição a uma request.
3. Você pode:
   - Selecionar uma **request existente** (caso tenha um aberto).
   - Criar uma **nova request** clicando no botão _"Criar"_.

### Criar Request Manualmente (SE09)

1. Acesse a transação `SE09` (Workbench).
2. Clique em **Criar** (`F8` ou ícone da folha em branco).
3. Escolha o tipo de request:
   - **Workbench**: para objetos de desenvolvimento (ABAP, Dicionário, etc.).
   - **Customizing**: para configurações do sistema.
4. Preencha:
   - **Descrição**.
   - **Responsável** (geralmente seu usuário).
5. Clique em **Salvar**.

---

## 🔍 Consultar Requests

### Usando a transação SE09

1. Acesse `SE09`.
2. Na tela inicial, filtre por:
   - **Usuário**: seu ID SAP.
   - **Status**: "Modificável" (abertos) ou "Liberado".
   - **Tipo de Request**: Workbench, Customizing ou ambos.
3. Clique em **Executar** (`F8`).

Você verá a lista de requests com os objetos associados.

---

## ✅ Liberar um Request

1. Acesse a transação `SE09`.
2. Localize o request desejado.
3. Clique duas vezes sobre o request para expandir os itens.
4. Clique com o botão direito no request (nível superior).
5. Selecione **Liberar (Release)**.
   - O sistema liberará primeiro as tarefas (se houver), depois o request principal.
6. Após liberação:
   - O request não poderá mais ser modificado.
   - Ele estará disponível para transporte entre ambientes.

> ⚠️ **Atenção**: só libere um request quando estiver 100% seguro de que os objetos estão corretos e prontos para transporte.

---

## 📌 Dicas úteis

- Requests seguem uma hierarquia: **Request principal > Tarefas**.
- Requests com status **Modificável** ainda podem ser editados.
- Requests **liberadas** não podem mais ser alteradas.
- Use a transação `SE01` para operações avançadas de gerenciamento.

[⬅️ Voltar ao Sumário Principal](README.md)

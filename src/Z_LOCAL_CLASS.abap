*=====================================================================*
* Report......: Z_CREATING_INTERNAL_TABLE                             *
* Author......: ESCHOEPF                                              *
* Date........: 24.11.2024 - 20:00                                    *
*---------------------------------------------------------------------*
* Description.:                                                       *
*---------------------------------------------------------------------*
* Date         Author       Description                               *
* 24.11.2024   ESCHOEPF     Creating a local class                    *
*=====================================================================*

CLASS lcl_local_class DEFINITION.

	PUBLIC SECTION.
	
		TYPES: BEGIN OF ty_user_login,
					username TYPE string,
					password TYPE string,  "Senha criptografada
					last_login TYPE timestamp,
			END OF ty_user_login.

		METHODS:
			constructor,
			register_user 
				IMPORTING 	
					iv_username TYPE string
					iv_password TYPE string
				RETURNING VALUE(rv_success) TYPE abap_bool,
			validate_login 
				IMPORTING 
					iv_username TYPE string
					iv_password TYPE string
				RETURNING VALUE(rv_valid) TYPE abap_bool.

	PRIVATE SECTION.
		DATA: mt_users TYPE STANDARD TABLE OF ty_user_login.
		
		METHODS:
			encrypt_password 
				IMPORTING 	
					iv_password TYPE string
				RETURNING VALUE(rv_encrypted) TYPE string.
		
	* PROTECTED SECTION.

ENDCLASS.


CLASS lcl_local_class IMPLEMENTATION.

	METHOD constructor.
		" Inicialização se necessário
	ENDMETHOD.

	METHOD encrypt_password.
		" Exemplo simples de criptografia
		" Em produção, use algoritmos mais seguros como SHA-256
		DATA: lv_hash TYPE hash160.

		CALL FUNCTION 'CALCULATE_HASH_FOR_RAW'
			EXPORTING
				data           = iv_password
			IMPORTING
				hash           = lv_hash
			EXCEPTIONS
				unknown_alg    = 1
				param_error    = 2
				internal_error = 3
				OTHERS         = 4.

		rv_encrypted = lv_hash.
	ENDMETHOD.

	METHOD register_user.
		DATA: ls_user TYPE ty_user_login.

		" Verifica se usuário já existe
		READ TABLE mt_users TRANSPORTING NO FIELDS 
			WITH KEY username = iv_username.
		IF sy-subrc = 0.
			rv_success = abap_false.
			RETURN.
		ENDIF.

		" Registra novo usuário
		ls_user-username   = iv_username.
		ls_user-password   = encrypt_password( iv_password ).
		ls_user-last_login = CONV timestamp( sy-datum && sy-uzeit ).

		INSERT ls_user INTO TABLE mt_users.
		rv_success = abap_true.
	ENDMETHOD.

	METHOD validate_login.
		DATA: ls_user TYPE ty_user_login,
			lv_encrypted_pwd TYPE string.

		" Verifica credenciais
		lv_encrypted_pwd = encrypt_password( iv_password ).
		
		READ TABLE mt_users INTO ls_user
			WITH KEY	username = iv_username
						password = lv_encrypted_pwd.
				
		rv_valid = COND #( WHEN sy-subrc = 0 THEN abap_true 
						   ELSE abap_false ).

		" Atualiza último login se válido
		IF rv_valid = abap_true.
			ls_user-last_login = CONV timestamp( sy-datum && sy-uzeit ).
			MODIFY TABLE mt_users FROM ls_user.
		ENDIF.
	ENDMETHOD.

ENDCLASS.
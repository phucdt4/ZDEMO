CLASS lsc_zr_ashopcart_000 DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.

    METHODS save_modified REDEFINITION.

ENDCLASS.

CLASS lsc_zr_ashopcart_000 IMPLEMENTATION.

  METHOD save_modified.
    DATA : lt_shopping_cart_as        TYPE STANDARD TABLE OF zashopcart_000.
    IF create-zr_ashopcart_000 IS NOT INITIAL.
      lt_shopping_cart_as = CORRESPONDING #( create-zr_ashopcart_000  MAPPING FROM ENTITY ).
      INSERT zashopcart_000 FROM TABLE @lt_shopping_cart_as.
    ENDIF.
    IF update IS NOT INITIAL.
      CLEAR lt_shopping_cart_as.
      lt_shopping_cart_as = CORRESPONDING #( update-zr_ashopcart_000 MAPPING FROM ENTITY ).
      LOOP AT update-zr_ashopcart_000  INTO DATA(shoppingcart) WHERE orderuuid IS NOT INITIAL.
        MODIFY zashopcart_000 FROM TABLE @lt_shopping_cart_as.
      ENDLOOP.
    ENDIF.


*************Add CONVERT KEY statement for Purchase Requisition create in case of EML implementation type**********
    IF update IS NOT INITIAL.
      DATA(creation_date) = cl_abap_context_info=>get_system_date(  ).

      LOOP AT update-zr_ashopcart_000 INTO DATA(onlineorder1) WHERE %control-overallstatus = if_abap_behv=>mk-on .
        LOOP AT zbp_r_ashopcart_000=>purchase_requisition_details INTO DATA(purchase_reqn_via_eml) WHERE  pid IS NOT INITIAL AND order_uuid = onlineorder1-orderuuid.
          CONVERT KEY OF i_purchaserequisitiontp FROM purchase_reqn_via_eml-pid TO DATA(ls_pr_key1).
          UPDATE zashopcart_000 SET purchase_requisition = @ls_pr_key1-purchaserequisition,
                                        pr_creation_date = @creation_date
                                        WHERE order_uuid = @purchase_reqn_via_eml-order_uuid.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
************END of CONVER KEY EML code snippet******************

    LOOP AT delete-zr_ashopcart_000 INTO DATA(shoppingcart_delete) WHERE orderuuid IS NOT INITIAL.
      DELETE FROM zashopcart_000 WHERE order_uuid = @shoppingcart_delete-orderuuid.
      DELETE FROM zashopcart_000_d WHERE orderuuid = @shoppingcart_delete-orderuuid.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS lhc_zr_ashopcart_000 DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_overall_status,
        new       TYPE string VALUE 'New / Composing',
*        composing  TYPE string VALUE 'Composing...',
        submitted TYPE string VALUE 'Submitted / Approved',
        cancelled TYPE string VALUE 'Cancelled',
      END OF c_overall_status.
    METHODS:
      get_global_authorizations FOR GLOBAL AUTHORIZATION
        IMPORTING
        REQUEST requested_authorizations FOR zr_ashopcart_000
        RESULT result,
      get_instance_features FOR INSTANCE FEATURES
        IMPORTING keys REQUEST requested_features FOR zr_ashopcart_000 RESULT result.

    METHODS calculatetotalprice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR zr_ashopcart_000~calculatetotalprice.

    METHODS setinitialordervalues FOR DETERMINE ON MODIFY
      IMPORTING keys FOR zr_ashopcart_000~setinitialordervalues.

    METHODS checkdeliverydate FOR VALIDATE ON SAVE
      IMPORTING keys FOR zr_ashopcart_000~checkdeliverydate.

    METHODS checkorderedquantity FOR VALIDATE ON SAVE
      IMPORTING keys FOR zr_ashopcart_000~checkorderedquantity.
    METHODS createpurchaserequisitionitem FOR MODIFY
      IMPORTING keys FOR ACTION zr_ashopcart_000~createpurchaserequisitionitem RESULT result.

    METHODS checkpurchaserequisition FOR VALIDATE ON SAVE
      IMPORTING keys FOR zr_ashopcart_000~checkpurchaserequisition.
ENDCLASS.

CLASS lhc_zr_ashopcart_000 IMPLEMENTATION.
  METHOD get_global_authorizations.
  ENDMETHOD.
  METHOD get_instance_features.

    " read relevant olineShop instance data
    READ ENTITIES OF zr_ashopcart_000 IN LOCAL MODE
      ENTITY zr_ashopcart_000
        FIELDS ( overallstatus )
        WITH CORRESPONDING #( keys )
      RESULT DATA(onlineorders)
      FAILED failed.

    " evaluate condition, set operation state, and set result parameter
    " update and checkout shall not be allowed as soon as purchase requisition has been created
    result = VALUE #( FOR onlineorder IN onlineorders
                      ( %tky                   = onlineorder-%tky
                        %features-%update
                          = COND #( WHEN onlineorder-overallstatus = c_overall_status-submitted  THEN if_abap_behv=>fc-o-disabled
                                    WHEN onlineorder-overallstatus = c_overall_status-cancelled THEN if_abap_behv=>fc-o-disabled
                                    ELSE if_abap_behv=>fc-o-enabled   )
*                         %features-%delete
*                           = COND #( WHEN OnlineOrder-PurchaseRequisition IS NOT INITIAL THEN if_abap_behv=>fc-o-disabled
*                                     WHEN OnlineOrder-OverallStatus = c_overall_status-cancelled THEN if_abap_behv=>fc-o-disabled
*                                     ELSE if_abap_behv=>fc-o-enabled   )
                        %action-edit
                          = COND #( WHEN onlineorder-overallstatus = c_overall_status-submitted THEN if_abap_behv=>fc-o-disabled
                                    WHEN onlineorder-overallstatus = c_overall_status-cancelled THEN if_abap_behv=>fc-o-disabled
                                    ELSE if_abap_behv=>fc-o-enabled   )

                        %action-createpurchaserequisitionitem
                          = COND #( WHEN onlineorder-overallstatus = c_overall_status-submitted OR onlineorder-%is_draft = if_abap_behv=>mk-on
                                    THEN if_abap_behv=>fc-o-disabled
                                    ELSE if_abap_behv=>fc-o-enabled )
                        ) ).
  ENDMETHOD.

  METHOD calculatetotalprice.
    DATA total_price TYPE zr_ashopcart_000-totalprice.

    " read transfered instances
    READ ENTITIES OF zr_ashopcart_000 IN LOCAL MODE
      ENTITY zr_ashopcart_000
        FIELDS ( orderid totalprice )
        WITH CORRESPONDING #( keys )
      RESULT DATA(onlineorders).

    LOOP AT onlineorders ASSIGNING FIELD-SYMBOL(<onlineorder>).
      " calculate total value
      <onlineorder>-totalprice = <onlineorder>-price * <onlineorder>-orderquantity.
    ENDLOOP.

    "update instances
    MODIFY ENTITIES OF zr_ashopcart_000 IN LOCAL MODE
      ENTITY zr_ashopcart_000
        UPDATE FIELDS ( totalprice )
        WITH VALUE #( FOR onlineorder IN onlineorders (
                          %tky       = onlineorder-%tky
                          totalprice = <onlineorder>-totalprice
                        ) ).
  ENDMETHOD.

  METHOD setinitialordervalues.

    DATA delivery_date TYPE i_purchasereqnitemtp-deliverydate.
    DATA(creation_date) = cl_abap_context_info=>get_system_date(  ).
    "set delivery date proposal
    delivery_date = cl_abap_context_info=>get_system_date(  ) + 14.
    "read transfered instances
    READ ENTITIES OF zr_ashopcart_000 IN LOCAL MODE
      ENTITY zr_ashopcart_000
        FIELDS ( orderid overallstatus  deliverydate )
        WITH CORRESPONDING #( keys )
      RESULT DATA(onlineorders).

    "delete entries with assigned order ID
    DELETE onlineorders WHERE orderid IS NOT INITIAL.
    CHECK onlineorders IS NOT INITIAL.

    " **Dummy logic to determine order IDs**
    " get max order ID from the relevant active and draft table entries
    SELECT MAX( order_id ) FROM zashopcart_000 INTO @DATA(max_order_id). "active table
    SELECT SINGLE FROM zashopcart_000_d FIELDS MAX( orderid ) INTO @DATA(max_orderid_draft). "draft table
    IF max_orderid_draft > max_order_id.
      max_order_id = max_orderid_draft.
    ENDIF.

    "set initial values of new instances
    MODIFY ENTITIES OF zr_ashopcart_000 IN LOCAL MODE
      ENTITY zr_ashopcart_000
        UPDATE FIELDS ( orderid overallstatus  deliverydate price  )
        WITH VALUE #( FOR order IN onlineorders INDEX INTO i (
                          %tky          = order-%tky
                          orderid       = max_order_id + i
                          overallstatus = c_overall_status-new  "'New / Composing'
                          deliverydate  = delivery_date
                          createdat     = creation_date
                        ) ).
    .
  ENDMETHOD.

  METHOD checkdeliverydate.

*   " read transfered instances
    READ ENTITIES OF zr_ashopcart_000 IN LOCAL MODE
      ENTITY zr_ashopcart_000
        FIELDS ( deliverydate )
        WITH CORRESPONDING #( keys )
      RESULT DATA(onlineorders).

    DATA(creation_date) = cl_abap_context_info=>get_system_date(  ).
    "raise msg if 0 > qty <= 10
    LOOP AT onlineorders INTO DATA(online_order).


      IF online_order-deliverydate IS INITIAL OR online_order-deliverydate = ' '.
        APPEND VALUE #( %tky = online_order-%tky ) TO failed-zr_ashopcart_000.
        APPEND VALUE #( %tky         = online_order-%tky
                        %state_area   = 'VALIDATE_DELIVERYDATE'
                        %msg          = new_message_with_text(
                                severity = if_abap_behv_message=>severity-error
                                text     = 'Delivery Date cannot be initial' )
                      ) TO reported-zr_ashopcart_000.

      ELSEIF  ( ( online_order-deliverydate ) - creation_date ) < 14.
        APPEND VALUE #(  %tky = online_order-%tky ) TO failed-zr_ashopcart_000.
        APPEND VALUE #(  %tky          = online_order-%tky
                        %state_area   = 'VALIDATE_DELIVERYDATE'
                        %msg          = new_message_with_text(
                                severity = if_abap_behv_message=>severity-error
                                text     = 'Delivery Date should be atleast 14 days after the creation date'  )

                        %element-orderquantity  = if_abap_behv=>mk-on
                      ) TO reported-zr_ashopcart_000.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD checkorderedquantity.

    "read relevant order instance data
    READ ENTITIES OF zr_ashopcart_000 IN LOCAL MODE
    ENTITY zr_ashopcart_000
    FIELDS ( orderid ordereditem orderquantity )
    WITH CORRESPONDING #( keys )
    RESULT DATA(onlineorders).

    "raise msg if 0 > qty <= 10
    LOOP AT onlineorders INTO DATA(onlineorder).
      APPEND VALUE #(  %tky           = onlineorder-%tky
                      %state_area    = 'VALIDATE_QUANTITY'
                    ) TO reported-zr_ashopcart_000.

      IF onlineorder-orderquantity IS INITIAL OR onlineorder-orderquantity = ' '.
        APPEND VALUE #( %tky = onlineorder-%tky ) TO failed-zr_ashopcart_000.
        APPEND VALUE #( %tky          = onlineorder-%tky
                        %state_area   = 'VALIDATE_QUANTITY'
                        %msg          = new_message_with_text(
                                severity = if_abap_behv_message=>severity-error
                                text     = 'Quantity cannot be empty' )
                        %element-orderquantity = if_abap_behv=>mk-on
                      ) TO reported-zr_ashopcart_000.

      ELSEIF onlineorder-orderquantity > 10.
        APPEND VALUE #(  %tky = onlineorder-%tky ) TO failed-zr_ashopcart_000.
        APPEND VALUE #(  %tky          = onlineorder-%tky
                        %state_area   = 'VALIDATE_QUANTITY'
                        %msg          = new_message_with_text(
                                severity = if_abap_behv_message=>severity-error
                                text     = 'Quantity should be below 10' )

                        %element-orderquantity  = if_abap_behv=>mk-on
                      ) TO reported-zr_ashopcart_000.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD createpurchaserequisitionitem.
    READ ENTITIES OF zr_ashopcart_000 IN LOCAL MODE
        ENTITY zr_ashopcart_000
          ALL FIELDS WITH
          CORRESPONDING #( keys )
        RESULT DATA(onlineorders).
***Integrate EML statement for Purchase Requisition create in case of released API implementation scenario ************
***Begin of Code Snippet********
    DATA: purchase_requisitions      TYPE TABLE FOR CREATE i_purchaserequisitiontp,
          purchase_requisition       TYPE STRUCTURE FOR CREATE i_purchaserequisitiontp,
          purchase_requisition_items TYPE TABLE FOR CREATE i_purchaserequisitiontp\_purchaserequisitionitem,
          purchase_requisition_item  TYPE STRUCTURE FOR CREATE i_purchaserequisitiontp\\purchaserequisition\_purchaserequisitionitem,
          delivery_date              TYPE i_purchasereqnitemtp-deliverydate,
          n                          TYPE i.
    LOOP AT onlineorders INTO DATA(onlineorder) WHERE overallstatus = c_overall_status-new .
*   SQL statement to include product and productgroup
      SELECT SINGLE FROM zi_products_000 FIELDS product, productgroup WHERE producttext = @onlineorder-ordereditem INTO @DATA(productdata) .
*
*      delivery_date = cl_abap_context_info=>get_system_date(  ) .
      delivery_date = cl_abap_context_info=>get_system_date(  ) + 14 .
      n += 1.
      "purchase requisition
      DATA(cid) = onlineorder-orderid && '_' && n.
      purchase_requisition = VALUE #(   %cid                      = cid
                                        purchaserequisitiontype   = 'NB'  ) .
      APPEND purchase_requisition TO purchase_requisitions.

      "purchase requisition item
      purchase_requisition_item = VALUE #(
                                        %cid_ref = cid
                                        %target  = VALUE #(  (
                                                      %cid                         = |My%ItemCID_{ n }|
                                                      plant                        = '1000'  "Plant 01 (DE)
                                                      accountassignmentcategory    = 'U'  "unknown
*                                                       PurchaseRequisitionItemText =  . "retrieved automatically from maintained MaterialInfo
                                                      requestedquantity            = onlineorder-orderquantity
                                                      purchaserequisitionprice     = onlineorder-price
                                                      purreqnitemcurrency          = onlineorder-currency
                                                      material                     = productdata-product

                                                      materialgroup               = productdata-productgroup
*                                                        Material                  = 'laptop'
*                                                       materialgroup              = 'system'
                                                      purchasinggroup             = '001'
*                                                       purchasingorganization     = '1010'
                                                      deliverydate                = delivery_date   "delivery_date  "yyyy-mm-dd (at least 10 days)
                                                      createdbyuser               = onlineorder-createdby
                                                      ) ) ).
      APPEND purchase_requisition_item TO purchase_requisition_items.
    ENDLOOP.
    IF keys IS NOT INITIAL .
      "purchase requisition
      MODIFY ENTITIES OF i_purchaserequisitiontp
        ENTITY purchaserequisition
          CREATE FIELDS ( purchaserequisitiontype )
          WITH purchase_requisitions
        "purchase requisition item
        CREATE BY \_purchaserequisitionitem
          FIELDS ( plant
*                  purchaserequisitionitemtext
                  accountassignmentcategory
                  requestedquantity
                  baseunit
                  purchaserequisitionprice
                  purreqnitemcurrency
                  material
                  materialgroup
                  purchasinggroup
                  purchasingorganization
                  deliverydate

                )
        WITH purchase_requisition_items
      REPORTED DATA(reported_create_pr)
      MAPPED DATA(mapped_create_pr)
      FAILED DATA(failed_create_pr).

      READ ENTITIES OF i_purchaserequisitiontp
      ENTITY purchaserequisition
      ALL FIELDS WITH CORRESPONDING #( mapped_create_pr-purchaserequisition )
      RESULT DATA(pr_result)
      FAILED DATA(pr_failed)
      REPORTED DATA(pr_reported).
    ENDIF.

    IF mapped_create_pr IS NOT INITIAL.
      LOOP AT onlineorders INTO DATA(onlineorder1) WHERE overallstatus = c_overall_status-new .
        LOOP AT mapped_create_pr-purchaserequisition INTO DATA(purchaserequisition_details)   .
          IF onlineorder1-orderid = substring_before( val = purchaserequisition_details-%cid sub = '_' ).

            APPEND VALUE #( cid                = purchaserequisition_details-%cid
                            pid                = purchaserequisition_details-%pid
                            order_uuid         = onlineorder1-orderuuid  ) TO zbp_r_ashopcart_000=>purchase_requisition_details .
            DELETE ADJACENT DUPLICATES FROM zbp_r_ashopcart_000=>purchase_requisition_details COMPARING pid.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

***End of EML Code Snippet********

      MODIFY ENTITIES OF zr_ashopcart_000 IN LOCAL MODE
        ENTITY zr_ashopcart_000
            UPDATE FIELDS ( overallstatus )
              WITH VALUE #( FOR key IN keys (
                orderuuid = key-orderuuid
                overallstatus = c_overall_status-submitted
            ) ).
ENDIF.
      "Read the changed data for action result
      READ ENTITIES OF zr_ashopcart_000 IN LOCAL MODE
        ENTITY zr_ashopcart_000
          ALL FIELDS WITH
          CORRESPONDING #( keys )
        RESULT DATA(result_read).
      "return result entities
      result = VALUE #( FOR result_order IN result_read ( %tky   = result_order-%tky
                                                          %param = result_order ) ).

  ENDMETHOD.

  METHOD checkpurchaserequisition.
  ENDMETHOD.

ENDCLASS.

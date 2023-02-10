CLASS lhc_Travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE Travel.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE Travel.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE Travel.

    METHODS read FOR READ
      IMPORTING keys FOR READ Travel RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK Travel.

    METHODS rba_Booking FOR READ
      IMPORTING keys_rba FOR READ Travel\_Booking FULL result_requested RESULT result LINK association_links.

    METHODS cba_Booking FOR MODIFY
      IMPORTING entities_cba FOR CREATE Travel\_Booking.

ENDCLASS.

CLASS lhc_Travel IMPLEMENTATION.

  METHOD create.

    DATA lt_messages   TYPE /dmo/t_message.
    DATA ls_legacy_entity_in  TYPE /dmo/travel.
    DATA lS_legacy_entity_out TYPE /dmo/travel.

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<ls_entity>).

      ls_legacy_entity_in = CORRESPONDING #( <ls_entity> MAPPING FROM ENTITY USING CONTROL ).

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_CREATE'
        EXPORTING
          is_travel   = CORRESPONDING /dmo/s_travel_in( ls_legacy_entity_in )
        IMPORTING
          es_travel   = lS_legacy_entity_out
          et_messages = lt_messages.

      IF lt_messages IS INITIAL.
        APPEND VALUE #( %cid = <ls_entity>-%cid travelid = lS_legacy_entity_out-travel_id ) TO mapped-travel.
      ELSE.

        "fill failed return structure for the framework
        APPEND VALUE #( travelid = ls_legacy_entity_in-travel_id ) TO failed-travel.

        "fill failed return structure for the framework
        LOOP AT lt_messages INTO DATA(message).

          "fill reported structure to be displayed on the UI
          APPEND VALUE #( travelid = ls_legacy_entity_in-travel_id
                          %msg = new_message( id = message-msgid
                                              number = message-msgno
                                              v1 = message-msgv1
                                              v2 = message-msgv2
                                              v3 = message-msgv3
                                              v4 = message-msgv4
                                              severity = CONV #( message-msgty ) )

         ) TO reported-travel.

        ENDLOOP.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD delete.

    DATA:
     lt_messages TYPE /dmo/t_message.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<ls_key>).

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_DELETE'
        EXPORTING
          iv_travel_id = <ls_key>-travelid
        IMPORTING
          et_messages  = lt_messages.

      IF lt_messages IS INITIAL.

        APPEND VALUE #( travelid = <ls_key>-travelid ) TO mapped-travel.

      ELSE.

        "fill failed return structure for the framework
        APPEND VALUE #( travelid = <ls_key>-travelid ) TO failed-travel.

        "fill failed return structure for the framework
        LOOP AT lt_messages INTO DATA(message).

          "fill reported structure to be displayed on the UI
          APPEND VALUE #( travelid = <ls_key>-travelid
                          %msg = new_message( id = message-msgid
                                              number = message-msgno
                                              v1 = message-msgv1
                                              v2 = message-msgv2
                                              v3 = message-msgv3
                                              v4 = message-msgv4
                                              severity = CONV #( message-msgty ) )


         ) TO reported-travel.

        ENDLOOP.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD update.

    DATA: ls_legacy_entity_in   TYPE /dmo/travel.
    DATA: ls_legacy_entity_x  TYPE /dmo/s_travel_inx . "refers to x structure (> BAPIs)
    DATA: lt_messages TYPE /dmo/t_message.

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<ls_entity>).

      ls_legacy_entity_in = CORRESPONDING #( <ls_entity> MAPPING FROM ENTITY ).
      ls_legacy_entity_x-travel_id = <ls_entity>-TravelID.
      ls_legacy_entity_x-_intx = CORRESPONDING zbc_s_rap_travel_x_007( <ls_entity> MAPPING FROM ENTITY ).

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_UPDATE'
        EXPORTING
          is_travel   = CORRESPONDING /dmo/s_travel_in( ls_legacy_entity_in )
          is_travelx  = ls_legacy_entity_x
        IMPORTING
          et_messages = lt_messages.

      IF lt_messages IS INITIAL.

        APPEND VALUE #( travelid = ls_legacy_entity_in-travel_id ) TO mapped-travel.

      ELSE.

        "fill failed return structure for the framework
        APPEND VALUE #( travelid = ls_legacy_entity_in-travel_id ) TO failed-travel.

        "fill failed return structure for the framework
        LOOP AT lt_messages INTO DATA(message).

          "fill reported structure to be displayed on the UI
          APPEND VALUE #( travelid = ls_legacy_entity_in-travel_id
                          %msg = new_message( id = message-msgid
                                              number = message-msgno
                                              v1 = message-msgv1
                                              v2 = message-msgv2
                                              v3 = message-msgv3
                                              v4 = message-msgv4
                                              severity = CONV #( message-msgty ) )


         ) TO reported-travel.

        ENDLOOP.
      ENDIF.

    ENDLOOP..

  ENDMETHOD.


  METHOD read.

    DATA: ls_legacy_entity_out TYPE /dmo/travel,
          lt_messages          TYPE /dmo/t_message.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<ls_key>) GROUP BY <ls_key>-TravelId.

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_READ'
        EXPORTING
          iv_travel_id = <ls_key>-travelid
        IMPORTING
          es_travel    = ls_legacy_entity_out
          et_messages  = lt_messages.

      IF lt_messages IS INITIAL.
        "fill result parameter with flagged fields

        INSERT CORRESPONDING #( ls_legacy_entity_out MAPPING TO ENTITY ) INTO TABLE result.

      ELSE.

        APPEND VALUE #( travelid = <ls_key>-travelid ) TO failed-travel.

        "fill failed return structure for the framework
        LOOP AT lt_messages ASSIGNING FIELD-SYMBOL(<ls_message>).

          "fill reported structure to be displayed on the UI
          APPEND VALUE #( travelid = <ls_key>-travelid
                          %msg = new_message( id = <ls_message>-msgid
                                              number = <ls_message>-msgno
                                              v1 = <ls_message>-msgv1
                                              v2 = <ls_message>-msgv2
                                              v3 = <ls_message>-msgv3
                                              v4 = <ls_message>-msgv4
                                              severity = CONV #( <ls_message>-msgty ) )


         ) TO reported-travel.

        ENDLOOP.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD lock.

    "Instantiate lock object
    DATA(lo_lock) = cl_abap_lock_object_factory=>get_instance( iv_name = '/DMO/ETRAVEL' ).


    LOOP AT keys ASSIGNING FIELD-SYMBOL(<ls_key>).

      TRY.
          "enqueue travel instance
          lo_lock->enqueue(
              it_parameter  = VALUE #( (  name = 'TRAVEL_ID' value = REF #( <ls_key>-travelid ) ) )
          ).
          "if foreign lock exists
        CATCH cx_abap_foreign_lock INTO DATA(lx_foreign_lock).

          "fill failed return structure for the framework
          APPEND VALUE #( travelid = <ls_key>-travelid ) TO failed-travel.
          "fill reported structure to be displayed on the UI
          APPEND VALUE #( travelid = <ls_key>-travelid
                          %msg = new_message( id = '/DMO/CM_FLIGHT_LEGAC'
                                              number = '032'
                                              v1 = <ls_key>-travelid
                                              v2 = lx_foreign_lock->user_name
                                              severity = CONV #( 'E' ) )
         ) TO reported-travel.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.

  METHOD rba_Booking.

    DATA: ls_legacy_parent_entity_out TYPE /dmo/travel,
          lt_legacy_entities_out      TYPE /dmo/t_booking,
          ls_entity                   LIKE LINE OF result,
          lt_message                  TYPE /dmo/t_message.


    LOOP AT keys_rba  ASSIGNING FIELD-SYMBOL(<ls_key_rba>) GROUP  BY <ls_key_rba>-TravelId.

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_READ'
        EXPORTING
          iv_travel_id = <ls_key_rba>-travelid
        IMPORTING
          es_travel    = ls_legacy_parent_entity_out
          et_booking   = lt_legacy_entities_out
          et_messages  = lt_message.

      IF lt_message IS INITIAL.

        LOOP AT lt_legacy_entities_out ASSIGNING FIELD-SYMBOL(<ls_booking>).
          "fill link table with key fields

          INSERT
            VALUE #(
                source-%key = <ls_key_rba>-%key
                target-%key = VALUE #(
                  TravelID  = <ls_booking>-travel_id
                  BookingID = <ls_booking>-booking_id
              )
            )
            INTO TABLE  association_links .

          "fill result parameter with flagged fields
          IF result_requested = abap_true.

            ls_entity = CORRESPONDING #( <ls_booking> MAPPING TO ENTITY ).
            INSERT ls_entity INTO TABLE result.

          ENDIF.

        ENDLOOP.

      ELSE.
        "fill failed table in case of error

        failed-travel = VALUE #(
          BASE failed-travel
          FOR msg IN lt_message (
            %key = <ls_key_rba>-TravelID
            %fail-cause = COND #(
              WHEN msg-msgty = 'E' AND  ( msg-msgno = '016' OR msg-msgno = '009' )
              THEN if_abap_behv=>cause-not_found
              ELSE if_abap_behv=>cause-unspecific
            )
          )
        ).

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD cba_Booking.

    DATA: lt_messages        TYPE /dmo/t_message,
          lt_booking_old     TYPE /dmo/t_booking,
          ls_entity          TYPE /dmo/booking,
          lv_last_booking_id TYPE /dmo/booking_id VALUE '0'.

    LOOP AT entities_cba ASSIGNING FIELD-SYMBOL(<ls_entity_cba>).

      DATA(lv_travelid) = <ls_entity_cba>-travelid.

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_READ'
        EXPORTING
          iv_travel_id = lv_travelid
        IMPORTING
          et_booking   = lt_booking_old
          et_messages  = lt_messages.

      IF lt_messages IS INITIAL.

        IF lt_booking_old IS NOT INITIAL.

          lv_last_booking_id = lt_booking_old[ lines( lt_booking_old ) ]-booking_id.

        ENDIF.

        LOOP AT <ls_entity_cba>-%target ASSIGNING FIELD-SYMBOL(<ls_entity>).

          ls_entity = CORRESPONDING #( <ls_entity> MAPPING FROM ENTITY USING CONTROL ) .

          lv_last_booking_id += 1.
          ls_entity-booking_id = lv_last_booking_id.

          CALL FUNCTION '/DMO/FLIGHT_TRAVEL_UPDATE'
            EXPORTING
              is_travel   = VALUE /dmo/s_travel_in( travel_id = lv_travelid )
              is_travelx  = VALUE /dmo/s_travel_inx( travel_id = lv_travelid )
              it_booking  = VALUE /dmo/t_booking_in( ( CORRESPONDING #( ls_entity ) ) )
              it_bookingx = VALUE /dmo/t_booking_inx(
                (
                  booking_id  = ls_entity-booking_id
                  action_code = /dmo/if_flight_legacy=>action_code-create
                )
              )
            IMPORTING
              et_messages = lt_messages.

          IF lt_messages IS INITIAL.

            INSERT
              VALUE #(
                %cid = <ls_entity>-%cid
                travelid = lv_travelid
                bookingid = ls_entity-booking_id
              )
              INTO TABLE mapped-booking.

          ELSE.

            INSERT VALUE #( %cid = <ls_entity>-%cid travelid = lv_travelid ) INTO TABLE failed-booking.

            LOOP AT lt_messages INTO DATA(message) WHERE msgty = 'E' OR msgty = 'A'.

              INSERT
                VALUE #(
                  %cid     = <ls_entity>-%cid
                  travelid = <ls_entity>-TravelID
                  %msg     = new_message(
                    id       = message-msgid
                    number   = message-msgno
                    severity = if_abap_behv_message=>severity-error
                    v1       = message-msgv1
                    v2       = message-msgv2
                    v3       = message-msgv3
                    v4       = message-msgv4
                  )
                )
                INTO TABLE reported-booking.

            ENDLOOP.

          ENDIF.

        ENDLOOP.

      ELSE.

        "fill failed return structure for the framework
        APPEND VALUE #( travelid = lv_travelid ) TO failed-travel.
        "fill reported structure to be displayed on the UI
        APPEND VALUE #( travelid = lv_travelid
                        %msg = new_message( id = lt_messages[ 1 ]-msgid
                                            number = lt_messages[ 1 ]-msgno
                                            v1 = lt_messages[ 1 ]-msgv1
                                            v2 = lt_messages[ 1 ]-msgv2
                                            v3 = lt_messages[ 1 ]-msgv3
                                            v4 = lt_messages[ 1 ]-msgv4
                                            severity = CONV #( lt_messages[ 1 ]-msgty ) )
       ) TO reported-travel.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZBC_I_RAP_TRAVEL_U_007 DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZBC_I_RAP_TRAVEL_U_007 IMPLEMENTATION.

  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.

    CALL FUNCTION '/DMO/FLIGHT_TRAVEL_SAVE'.

  ENDMETHOD.

  METHOD cleanup.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.

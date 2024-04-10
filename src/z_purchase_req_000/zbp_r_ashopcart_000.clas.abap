class ZBP_R_ASHOPCART_000 definition
  public
  abstract
  final
  for behavior of ZR_ASHOPCART_000 .

public section.
TYPES: BEGIN OF ty_pr_details,
            pid        TYPE abp_behv_pid,
            cid        TYPE abp_behv_cid,
            pur_req    TYPE zashopcart_000-purchase_requisition,
            order_uuid TYPE zashopcart_000-order_uuid,
          END OF ty_pr_details.
    CLASS-DATA purchase_requisition_details TYPE TABLE OF ty_pr_details.
protected section.
private section.
ENDCLASS.



CLASS ZBP_R_ASHOPCART_000 IMPLEMENTATION.
ENDCLASS.

-- =====================================================
-- Customer Management Database Schema
-- =====================================================

-- Create Customers Table
CREATE TABLE CUSTOMERS (
    CUST_ID         NUMBER(6) PRIMARY KEY,
    CUST_NAME       VARCHAR2(30) NOT NULL,
    BALANCE         NUMBER(9,2) NOT NULL,
    AGE             NUMBER(3) NOT NULL,
    STATUS          CHAR(1) NOT NULL,
    CREATED_DATE    DATE DEFAULT SYSDATE,
    UPDATED_DATE    DATE,

    -- Business Rule: Status must be 'A' (Active) or 'I' (Inactive)
    CONSTRAINT chk_status CHECK (STATUS IN ('A', 'I')),

    -- Business Rule: Balance cannot be negative
    CONSTRAINT chk_balance CHECK (BALANCE >= 0),

    -- Business Rule: Age must be reasonable
    CONSTRAINT chk_age CHECK (AGE BETWEEN 0 AND 150),

    -- Business Rule: Minimum balance requirement
    CONSTRAINT chk_min_balance CHECK (BALANCE >= 1000.00)
);

-- Create index for faster lookups
CREATE INDEX idx_customer_status ON CUSTOMERS(STATUS);
CREATE INDEX idx_customer_balance ON CUSTOMERS(BALANCE);

-- =====================================================
-- Stored Procedure: Calculate and Apply Discount
-- =====================================================
CREATE OR REPLACE PROCEDURE UPDATE_CUSTOMER_DISCOUNT(
    p_cust_id IN NUMBER,
    p_discount_rate OUT NUMBER
)
IS
    v_age NUMBER(3);
    v_balance NUMBER(9,2);
    v_status CHAR(1);
    v_discount_amount NUMBER(9,2);
    v_final_balance NUMBER(9,2);

    -- Business Constants
    c_senior_age CONSTANT NUMBER := 65;
    c_senior_discount CONSTANT NUMBER := 0.20;
    c_regular_discount CONSTANT NUMBER := 0.10;
    c_min_balance CONSTANT NUMBER := 1000.00;

BEGIN
    -- Get customer details
    SELECT AGE, BALANCE, STATUS
    INTO v_age, v_balance, v_status
    FROM CUSTOMERS
    WHERE CUST_ID = p_cust_id;

    -- Business Rule: Senior citizens get 20% discount
    IF v_age >= c_senior_age THEN
        p_discount_rate := c_senior_discount;
    -- Business Rule: Active customers get 10% discount
    ELSIF v_status = 'A' THEN
        p_discount_rate := c_regular_discount;
    ELSE
        p_discount_rate := 0;
    END IF;

    -- Calculate discount amount
    v_discount_amount := v_balance * p_discount_rate;
    v_final_balance := v_balance - v_discount_amount;

    -- Business Rule: Validate minimum balance
    IF v_final_balance >= c_min_balance THEN
        UPDATE CUSTOMERS
        SET BALANCE = v_final_balance,
            UPDATED_DATE = SYSDATE
        WHERE CUST_ID = p_cust_id;

        COMMIT;
    ELSE
        RAISE_APPLICATION_ERROR(-20001,
            'Balance would fall below minimum requirement');
    END IF;

EXCEPTION
    WHEN NO_DATA_FOUND THEN
        RAISE_APPLICATION_ERROR(-20002, 'Customer not found');
    WHEN OTHERS THEN
        ROLLBACK;
        RAISE;
END;
/

-- =====================================================
-- Function: Get Discount Tier
-- =====================================================
CREATE OR REPLACE FUNCTION GET_DISCOUNT_TIER(
    p_age IN NUMBER
) RETURN VARCHAR2
IS
    v_tier VARCHAR2(10);
BEGIN
    -- Business Rule: Determine discount tier by age
    IF p_age >= 65 THEN
        v_tier := 'SENIOR';
    ELSIF p_age >= 18 THEN
        v_tier := 'REGULAR';
    ELSE
        v_tier := 'YOUTH';
    END IF;

    RETURN v_tier;
END;
/

-- =====================================================
-- Trigger: Audit Balance Changes
-- =====================================================
CREATE OR REPLACE TRIGGER TRG_CUSTOMER_AUDIT
AFTER UPDATE OF BALANCE ON CUSTOMERS
FOR EACH ROW
BEGIN
    -- Log balance changes for auditing
    INSERT INTO CUSTOMER_AUDIT (
        CUST_ID,
        OLD_BALANCE,
        NEW_BALANCE,
        CHANGE_DATE
    ) VALUES (
        :NEW.CUST_ID,
        :OLD.BALANCE,
        :NEW.BALANCE,
        SYSDATE
    );
END;
/

-- =====================================================
-- Views
-- =====================================================

-- View: High Balance Customers
CREATE OR REPLACE VIEW VW_HIGH_BALANCE_CUSTOMERS AS
SELECT
    CUST_ID,
    CUST_NAME,
    BALANCE,
    AGE,
    STATUS
FROM CUSTOMERS
WHERE BALANCE > 10000;

-- View: Senior Customers
CREATE OR REPLACE VIEW VW_SENIOR_CUSTOMERS AS
SELECT
    CUST_ID,
    CUST_NAME,
    BALANCE,
    AGE,
    GET_DISCOUNT_TIER(AGE) AS DISCOUNT_TIER
FROM CUSTOMERS
WHERE AGE >= 65;

-- =====================================================
-- Sample Queries
-- =====================================================

-- Get all active customers with high balance
SELECT * FROM CUSTOMERS
WHERE STATUS = 'A'
  AND BALANCE > 5000
ORDER BY BALANCE DESC;

-- Calculate total balance by status
SELECT
    STATUS,
    COUNT(*) AS CUSTOMER_COUNT,
    SUM(BALANCE) AS TOTAL_BALANCE,
    AVG(BALANCE) AS AVG_BALANCE
FROM CUSTOMERS
GROUP BY STATUS;

-- Find customers eligible for senior discount
SELECT
    CUST_NAME,
    AGE,
    BALANCE,
    BALANCE * 0.20 AS DISCOUNT_AMOUNT
FROM CUSTOMERS
WHERE AGE >= 65
  AND STATUS = 'A';

-- Update customer balance (with business rule validation)
UPDATE CUSTOMERS
SET BALANCE = BALANCE - 500,
    UPDATED_DATE = SYSDATE
WHERE CUST_ID = 123456
  AND BALANCE - 500 >= 1000;  -- Ensure minimum balance

-- Complex join query
SELECT
    c.CUST_NAME,
    c.BALANCE,
    t.TRANSACTION_DATE,
    t.AMOUNT
FROM CUSTOMERS c
INNER JOIN TRANSACTIONS t ON c.CUST_ID = t.CUST_ID
WHERE c.STATUS = 'A'
  AND t.TRANSACTION_DATE >= SYSDATE - 30
ORDER BY t.TRANSACTION_DATE DESC;

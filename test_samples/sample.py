"""
Customer Service Module
Handles customer business logic and discount calculations
"""
from decimal import Decimal
from typing import Optional
from datetime import datetime
import sqlite3


# Business constants
SENIOR_DISCOUNT = Decimal('0.20')
REGULAR_DISCOUNT = Decimal('0.10')
SENIOR_AGE = 65
MIN_BALANCE = Decimal('1000.00')


class Customer:
    """Customer entity class"""

    def __init__(self, id: int, name: str, balance: Decimal, age: int, active: bool):
        self.id = id
        self.name = name
        self.balance = balance
        self.age = age
        self.active = active

    def __repr__(self):
        return f"Customer(id={self.id}, name='{self.name}', balance={self.balance})"


class CustomerService:
    """Service class for customer operations"""

    def __init__(self, db_connection):
        self.conn = db_connection

    def calculate_discount(self, customer: Customer) -> Decimal:
        """
        Calculate discount based on customer age and status

        Business Rules:
        - Senior citizens (65+) get 20% discount
        - Active customers get 10% discount
        - Inactive customers get no discount
        """
        if customer.age >= SENIOR_AGE:
            return SENIOR_DISCOUNT
        elif customer.active:
            return REGULAR_DISCOUNT
        else:
            return Decimal('0')

    def apply_discount(self, balance: Decimal, discount_rate: Decimal) -> Decimal:
        """Apply discount to customer balance"""
        discount_amount = balance * discount_rate
        return balance - discount_amount

    def validate_balance(self, balance: Decimal) -> bool:
        """
        Validate minimum balance requirement

        Business Rule: Minimum balance must be maintained
        """
        if balance < MIN_BALANCE:
            print(f"WARNING: Balance {balance} is below minimum {MIN_BALANCE}")
            return False
        return True

    def update_customer_balance(self, customer_id: int, new_balance: Decimal):
        """Update customer balance in database"""
        cursor = self.conn.cursor()
        cursor.execute(
            "UPDATE customers SET balance = ? WHERE id = ?",
            (float(new_balance), customer_id)
        )
        self.conn.commit()

    def get_high_balance_customers(self, threshold: Decimal) -> list[Customer]:
        """Get customers with balance above threshold"""
        cursor = self.conn.cursor()
        cursor.execute(
            "SELECT id, name, balance, age, active FROM customers WHERE balance > ?",
            (float(threshold),)
        )

        customers = []
        for row in cursor.fetchall():
            customers.append(Customer(
                id=row[0],
                name=row[1],
                balance=Decimal(str(row[2])),
                age=row[3],
                active=bool(row[4])
            ))

        return customers

    def process_customer_discount(self, customer: Customer):
        """
        Process customer discount workflow

        Steps:
        1. Calculate applicable discount
        2. Apply discount to balance
        3. Validate final balance
        4. Update database if valid
        """
        discount_rate = self.calculate_discount(customer)
        final_balance = self.apply_discount(customer.balance, discount_rate)

        if self.validate_balance(final_balance):
            self.update_customer_balance(customer.id, final_balance)
            print(f"Processed discount for {customer.name}: {customer.balance} -> {final_balance}")
        else:
            raise ValueError(f"Balance validation failed for customer: {customer.name}")

    def get_customer_by_id(self, customer_id: int) -> Optional[Customer]:
        """Retrieve customer by ID"""
        cursor = self.conn.cursor()
        cursor.execute(
            "SELECT id, name, balance, age, active FROM customers WHERE id = ?",
            (customer_id,)
        )

        row = cursor.fetchone()
        if row:
            return Customer(
                id=row[0],
                name=row[1],
                balance=Decimal(str(row[2])),
                age=row[3],
                active=bool(row[4])
            )
        return None


def calculate_age_discount_tier(age: int) -> str:
    """
    Determine discount tier based on age

    Business Rules:
    - Age 65+: Senior tier
    - Age 18-64: Regular tier
    - Age <18: Youth tier
    """
    if age >= 65:
        return "SENIOR"
    elif age >= 18:
        return "REGULAR"
    else:
        return "YOUTH"


# Example usage
if __name__ == "__main__":
    # Create database connection
    conn = sqlite3.connect(':memory:')

    # Initialize service
    service = CustomerService(conn)

    # Create sample customer
    customer = Customer(
        id=1,
        name="John Doe",
        balance=Decimal('5000.00'),
        age=70,
        active=True
    )

    # Process discount
    try:
        service.process_customer_discount(customer)
    except ValueError as e:
        print(f"Error: {e}")

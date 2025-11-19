package com.example.banking;

import java.math.BigDecimal;
import java.sql.*;
import java.time.LocalDate;
import java.util.List;
import javax.validation.constraints.*;

/**
 * Customer Service - Handles customer business logic
 */
public class CustomerService {

    private static final BigDecimal SENIOR_DISCOUNT = new BigDecimal("0.20");
    private static final BigDecimal REGULAR_DISCOUNT = new BigDecimal("0.10");
    private static final int SENIOR_AGE = 65;
    private static final BigDecimal MIN_BALANCE = new BigDecimal("1000.00");

    private Connection connection;

    public CustomerService(Connection connection) {
        this.connection = connection;
    }

    /**
     * Calculate discount based on customer age and status
     * Business Rule: Senior citizens (65+) get 20% discount
     * Business Rule: Active customers get 10% discount
     */
    public BigDecimal calculateDiscount(Customer customer) {
        if (customer.getAge() >= SENIOR_AGE) {
            return SENIOR_DISCOUNT;
        } else if (customer.isActive()) {
            return REGULAR_DISCOUNT;
        }
        return BigDecimal.ZERO;
    }

    /**
     * Apply discount to customer balance
     */
    public BigDecimal applyDiscount(BigDecimal balance, BigDecimal discountRate) {
        BigDecimal discountAmount = balance.multiply(discountRate);
        return balance.subtract(discountAmount);
    }

    /**
     * Validate minimum balance requirement
     * Business Rule: Minimum balance must be maintained
     */
    public boolean validateBalance(BigDecimal balance) {
        if (balance.compareTo(MIN_BALANCE) < 0) {
            System.out.println("WARNING: Balance below minimum");
            return false;
        }
        return true;
    }

    /**
     * Update customer balance in database
     */
    public void updateCustomerBalance(int customerId, BigDecimal newBalance) throws SQLException {
        String sql = "UPDATE CUSTOMERS SET BALANCE = ? WHERE CUST_ID = ?";

        try (PreparedStatement stmt = connection.prepareStatement(sql)) {
            stmt.setBigDecimal(1, newBalance);
            stmt.setInt(2, customerId);
            stmt.executeUpdate();
        }
    }

    /**
     * Get customers with balance above threshold
     */
    public List<Customer> getHighBalanceCustomers(BigDecimal threshold) throws SQLException {
        String query = "SELECT * FROM CUSTOMERS WHERE BALANCE > ?";

        try (PreparedStatement stmt = connection.prepareStatement(query)) {
            stmt.setBigDecimal(1, threshold);
            ResultSet rs = stmt.executeQuery();

            // Process results...
            return processResultSet(rs);
        }
    }

    /**
     * Process customer discount workflow
     */
    public void processCustomerDiscount(Customer customer) throws SQLException {
        BigDecimal discountRate = calculateDiscount(customer);
        BigDecimal finalBalance = applyDiscount(customer.getBalance(), discountRate);

        if (validateBalance(finalBalance)) {
            updateCustomerBalance(customer.getId(), finalBalance);
        } else {
            throw new IllegalStateException("Balance validation failed for customer: " + customer.getName());
        }
    }

    private List<Customer> processResultSet(ResultSet rs) throws SQLException {
        // Implementation details...
        return null;
    }
}

/**
 * Customer entity class
 */
class Customer {
    @NotNull
    private Integer id;

    @NotEmpty
    @Size(max = 30)
    private String name;

    @NotNull
    @Min(0)
    private BigDecimal balance;

    @Min(0)
    @Max(150)
    private int age;

    private boolean active;

    // Getters
    public Integer getId() { return id; }
    public String getName() { return name; }
    public BigDecimal getBalance() { return balance; }
    public int getAge() { return age; }
    public boolean isActive() { return active; }

    // Setters
    public void setId(Integer id) { this.id = id; }
    public void setName(String name) { this.name = name; }
    public void setBalance(BigDecimal balance) { this.balance = balance; }
    public void setAge(int age) { this.age = age; }
    public void setActive(boolean active) { this.active = active; }
}

package io.github.luminion.sqlbooster.enums;

import lombok.Getter;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * SQL 操作符枚举.
 * <p>
 * 优化点：
 * 1. 内置别名定义，消除硬编码 switch.
 * 2. 使用 HashMap 进行 O(1) 查找.
 * 3. 提供面向对象的判断方法 (isLike, isCompare).
 *
 * @author luminion
 * @since 1.0.0
 */
@Getter
public enum SqlKeyword {

    // ==================== 连接符 ====================
    AND("AND", "AND", "&", "&&"),
    OR("OR", "OR", "|", "||"),

    // ==================== 比较符 ====================
    EQ("=", "=", "==", "EQ"),
    NE("<>", "<>", "!=", "NE"),
    LT("<", "<", "LT"),
    LTE("<=", "<=", "LE", "LTE"), // 支持 LE 和 LTE
    GT(">", ">", "GT"),
    GTE(">=", ">=", "GE", "GTE"), // 支持 GE 和 GTE

    // ==================== 模糊/范围 ====================
    LIKE("LIKE", "LIKE"),
    NOT_LIKE("NOT LIKE", "NOT LIKE", "NOT_LIKE"),
    IN("IN", "IN"),
    NOT_IN("NOT IN", "NOT IN", "NOT_IN"),

    // ==================== Null 判断 ====================
    IS_NULL("IS NULL", "IS NULL", "IS_NULL"),
    IS_NOT_NULL("IS NOT NULL", "IS NOT NULL", "IS_NOT_NULL"),

    // ==================== 位运算 (XML特有) ====================
    BIT_ANY("BIT ANY", "BIT ANY", "BIT_ANY"),
    BIT_ALL("BIT ALL", "BIT ALL", "BIT_ALL"),
    BIT_NONE("BIT NONE", "BIT NONE", "BIT_NONE");

    /**
     * 标准 SQL 关键字 (用于拼装 SQL).
     */
    private final String symbol;

    /**
     * 支持的所有别名 (用于解析输入).
     */
    private final String[] aliases;

    SqlKeyword(String symbol, String... aliases) {
        this.symbol = symbol;
        this.aliases = aliases;
    }

    // ==================== 静态查找逻辑 ====================

    private static final Map<String, SqlKeyword> LOOKUP_MAP;

    static {
        Map<String, SqlKeyword> map = new HashMap<>();
        for (SqlKeyword item : values()) {
            // 注册所有别名 (转大写)
            for (String alias : item.aliases) {
                map.put(alias.toUpperCase(), item);
            }
        }
        LOOKUP_MAP = Collections.unmodifiableMap(map);
    }

    /**
     * 解析操作符字符串为枚举对象.
     * <p>相当于原来的 replaceOperator + replaceConnector，统一了逻辑.</p>
     *
     * @param operator 操作符字符串 (e.g. "eq", "==", ">=")
     * @return 对应的 SqlKeyword
     * @throws IllegalArgumentException 当操作符不支持时抛出
     */
    public static SqlKeyword resolve(String operator) {
        if (operator == null || operator.isEmpty()) {
            return EQ; // 默认行为保持一致
        }
        SqlKeyword match = LOOKUP_MAP.get(operator.toUpperCase());
        if (match == null) {
            throw new IllegalArgumentException("Illegal operator: " + operator);
        }
        return match;
    }

    /**
     * 兼容旧方法的入口：解析并返回标准关键字字符串.
     *
     * @param operator 操作符
     * @return 标准 Keyword (e.g. "=")
     */
    public static String normalize(String operator) {
        return resolve(operator).getSymbol();
    }

    // ==================== 实例判断方法 (面向对象) ====================

    /**
     * 是否为连接符 (AND/OR).
     */
    public boolean isConnector() {
        return this == AND || this == OR;
    }

    /**
     * 是否为相等判断 (EQ/NE).
     */
    public boolean isEquality() {
        return this == EQ || this == NE;
    }

    /**
     * 是否为大小比较 (<, <=, >, >=).
     */
    public boolean isCompare() {
        return this == LT || this == LTE || this == GT || this == GTE;
    }

    /**
     * 是否为 LIKE 系列.
     */
    public boolean isLike() {
        return this == LIKE || this == NOT_LIKE;
    }

    /**
     * 是否为 IN 系列.
     */
    public boolean isIn() {
        return this == IN || this == NOT_IN;
    }

    /**
     * 是否为 NULL 判断系列.
     */
    public boolean isNullCheck() {
        return this == IS_NULL || this == IS_NOT_NULL;
    }

    /**
     * 是否为位运算系列.
     */
    public boolean isBitOperation() {
        return this == BIT_ANY || this == BIT_ALL || this == BIT_NONE;
    }
}
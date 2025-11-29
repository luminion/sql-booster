package io.github.luminion.sqlbooster.enums;

import lombok.Getter;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * SQL 操作符枚举。
 * <p>
 * 设计目标：
 * 1. 内置别名，消除硬编码的 switch-case。
 * 2. 使用 HashMap 提供 O(1) 的快速查找。
 * 3. 提供面向对象的判断方法 (例如 isLike, isCompare)。
 */
@Getter
public enum SqlKeyword {

    // ==================== 比较符 ====================
    EQ("=", "=", "==", "EQ"),
    NE("<>", "<>", "!=", "NE"),
    LT("<", "<", "LT"),
    LTE("<=", "<=", "LE", "LTE"),
    GT(">", ">", "GT"),
    GTE(">=", ">=", "GE", "GTE"),

    // ==================== 模糊 ====================
    LIKE("LIKE", "LIKE"),
    NOT_LIKE("NOT LIKE", "NOT LIKE", "NOT_LIKE"),

    // ==================== 范围 ====================
    IN("IN", "IN"),
    NOT_IN("NOT IN", "NOT IN", "NOT_IN"),

    // ==================== Null 判断 ====================
    IS_NULL("IS NULL", "IS NULL", "IS_NULL"),
    IS_NOT_NULL("IS NOT NULL", "IS NOT NULL", "IS_NOT_NULL"),

    // ==================== 位运算 (XML特有) ====================
    BIT_ANY("BIT ANY", "BIT ANY", "BIT_ANY"),
    BIT_ALL("BIT ALL", "BIT ALL", "BIT_ALL"),
    BIT_NONE("BIT NONE", "BIT NONE", "BIT_NONE");

    private final String symbol;
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
     * 将操作符字符串（如 "eq", "=="）解析为对应的枚举对象。
     *
     * @param operator 操作符字符串
     * @return 对应的 SqlKeyword
     * @throws IllegalArgumentException 当操作符不支持时抛出
     */
    public static SqlKeyword resolve(String operator) {
        if (operator == null || operator.isEmpty()) {
            return EQ; // 默认返回 EQ
        }
        SqlKeyword match = LOOKUP_MAP.get(operator.toUpperCase());
        if (match == null) {
            throw new IllegalArgumentException("Illegal operator: " + operator);
        }
        return match;
    }

    /**
     * 将操作符字符串解析并返回其标准 SQL 关键字。
     *
     * @param operator 操作符
     * @return 标准 Keyword (e.g. "=")
     */
    public static String normalize(String operator) {
        return resolve(operator).getSymbol();
    }

    // ==================== 实例判断方法 ====================

    public boolean isEquality() {
        return this == EQ || this == NE;
    }

    public boolean isCompare() {
        return this == LT || this == LTE || this == GT || this == GTE;
    }

    public boolean isLike() {
        return this == LIKE || this == NOT_LIKE;
    }

    public boolean isIn() {
        return this == IN || this == NOT_IN;
    }

    public boolean isNullCheck() {
        return this == IS_NULL || this == IS_NOT_NULL;
    }

    public boolean isBitOperation() {
        return this == BIT_ANY || this == BIT_ALL || this == BIT_NONE;
    }
}

package io.github.luminion.sqlbooster.enums;

import io.github.luminion.sqlbooster.util.StrUtils;
import lombok.Getter;

/**
 * 查询字段后缀与 SQL 操作符的映射枚举。
 * <p>
 * 用于解析方法或参数名，自动支持两种风格：
 * 1. 驼峰风格 (e.g., "nameEq", "ageGt")
 * 2. 下划线风格 (e.g., "name_eq", "age_gt")
 * </p>
 */
public enum ConditionSuffix {
    NE(SqlKeyword.NE, "Ne"),
    LT(SqlKeyword.LT, "Lt"),
    LTE(SqlKeyword.LTE, "Lte"),
    GT(SqlKeyword.GT, "Gt"),
    GTE(SqlKeyword.GTE, "Gte"),

    LIKE(SqlKeyword.LIKE, "Like"),
    NOT_LIKE(SqlKeyword.NOT_LIKE, "NotLike"),

    IN(SqlKeyword.IN, "In"),
    NOT_IN(SqlKeyword.NOT_IN, "NotIn"),

    IS_NULL(SqlKeyword.IS_NULL, "IsNull"),
    IS_NOT_NULL(SqlKeyword.IS_NOT_NULL, "IsNotNull"),

    BIT_ANY(SqlKeyword.BIT_ANY, "BitAny"),
    BIT_ALL(SqlKeyword.BIT_ALL, "BitAll"),
    BIT_NONE(SqlKeyword.BIT_NONE, "BitNone");

    private final SqlKeyword sqlKeyword;
    @Getter
    private final String camelCase;
    @Getter
    private final String underscore;

    ConditionSuffix(SqlKeyword sqlKeyword, String camelCase) {
        this.camelCase = camelCase;
        this.sqlKeyword = sqlKeyword;
        // 自动生成下划线风格: "NotLike" -> "_not_like"
        this.underscore = StrUtils.camelCaseToUnderscore(camelCase);
    }

    public String getKeyword() {
        return sqlKeyword.getSymbol();
    }
}

package io.github.luminion.sqlbooster.model.api;

import io.github.luminion.sqlbooster.model.enums.SqlKeyword;
import lombok.*;

/**
 * SQL 条件实体类.
 * <p>
 * 实现了 {@link Condition} 接口, 用于表示 SQL 查询中的单个条件, 包括字段名、操作符和值.
 *
 * @author luminion
 * @since 1.0.0
 */
@Data
@NoArgsConstructor
public class Condition {

    /**
     * 字段名.
     */
    protected String field;
    /**
     * 操作符.
     */
    protected String operator = SqlKeyword.EQ.getKeyword();
    /**
     * 条件值.
     */
    protected Object value = "";

    public Condition(String field, Object value) {
        this.field = field;
        this.value = value;
    }

    public Condition(String field, String operator, Object value) {
        this.field = field;
        this.operator = SqlKeyword.replaceOperator(operator);
        this.value = value;
    }

}
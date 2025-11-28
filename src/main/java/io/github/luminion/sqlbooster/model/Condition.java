package io.github.luminion.sqlbooster.model;

import io.github.luminion.sqlbooster.enums.SqlKeyword;
import lombok.*;

/**
 * SQL 条件实体类.
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
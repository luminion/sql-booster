package io.github.luminion.sqlbooster.model.query;

import io.github.luminion.sqlbooster.enums.SqlKeyword;
import lombok.*;

import java.io.Serializable;

/**
 * 单个查询条件。
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class Condition implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * 条件对应的字段名。
     */
    protected String field;

    /**
     * SQL 操作符，例如 "="、">"、"LIKE"。
     */
    protected String operator = SqlKeyword.EQ.getSymbol();

    /**
     * 条件的值。
     */
    protected Object value = "";

}

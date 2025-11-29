package io.github.luminion.sqlbooster.model.query;

import lombok.*;

import java.io.Serializable;

/**
 * SQL 排序实体类.
 *
 * @author luminion
 * @since 1.0.0
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class Sort implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * 排序字段.
     */
    protected String field;

    /**
     * 是否为升序排列.
     */
    protected boolean asc;

}
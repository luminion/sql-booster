package io.github.luminion.sqlbooster.model;

import lombok.*;

/**
 * SQL 排序实体类.
 *
 * @author luminion
 * @since 1.0.0
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class Sort {

    /**
     * 排序字段.
     */
    protected String field;

    /**
     * 是否为升序排列.
     */
    protected boolean asc;

}
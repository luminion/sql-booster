package io.github.luminion.sqlbooster.model;

import io.github.luminion.sqlbooster.model.query.ConditionSegment;
import io.github.luminion.sqlbooster.model.query.Sort;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;

/**
 * 查询参数封装类
 *
 * @param <T> 实体类型
 * @author luminion
 * @since 1.0.0
 */
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@AllArgsConstructor
public class SqlContext<T> extends ConditionSegment {

    /**
     * 排序字段列表.
     */
    protected LinkedHashSet<Sort> sorts = new LinkedHashSet<>();

    /**
     * 非本表字段的额外条件.
     */
    protected Map<String, Object> params = new HashMap<>();

    @Override
    public ConditionSegment merge(ConditionSegment conditionSegment) {
        if (conditionSegment == null) {
            return this;
        }
        if (conditionSegment instanceof SqlContext) {
            SqlContext<?> sqlContext = (SqlContext<?>) conditionSegment;
            this.sorts.addAll(sqlContext.getSorts());
            this.params.putAll(sqlContext.getParams());
        }
        return super.merge(conditionSegment);
    }
}
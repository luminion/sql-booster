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
 * SQL 查询的上下文容器。
 * <p>
 * 封装了查询条件、排序规则以及其他动态参数。
 *
 * @param <T> 实体类型
 */
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@AllArgsConstructor
public class SqlContext<T> extends ConditionSegment {
    private static final long serialVersionUID = 1L;

    /**
     * 排序字段列表。
     */
    protected LinkedHashSet<Sort> sorts = new LinkedHashSet<>();

    /**
     * 动态参数，用于存储未被直接解析为 Condition 的查询参数。
     * <p>
     * 主要用于 XML 中自定义的 SQL 逻辑。
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

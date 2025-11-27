package io.github.luminion.sqlbooster.model.api;

import io.github.luminion.sqlbooster.core.BoosterParam;
import lombok.Data;
import lombok.EqualsAndHashCode;

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
public class SqlContext<T> extends ConditionNode implements BoosterParam<T> {

    /**
     * 排序字段列表.
     */
    protected LinkedHashSet<Sort> sorts = new LinkedHashSet<>();

    /**
     * 非本表字段的额外条件.
     */
    protected transient Map<String, Object> extra = new HashMap<>();
    
    @Override
    protected ConditionNode merge(ConditionNode conditionNode) {
        if (conditionNode == null) {
            return this;
        }
        if (conditionNode instanceof SqlContext) {
            SqlContext<?> sqlContext = (SqlContext<?>) conditionNode;
            this.sorts.addAll(sqlContext.getSorts());
        }
        return super.merge(conditionNode);
    }
}
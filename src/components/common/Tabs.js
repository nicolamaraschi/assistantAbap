import React from 'react';
import styled from 'styled-components';

// Componente Tabs riutilizzabile
const Tabs = ({ 
  activeTab, 
  onChange, 
  tabs = [],
  variant = 'default'
}) => {
  return (
    <TabsContainer variant={variant}>
      {tabs.map((tab) => (
        <Tab
          key={tab.id}
          active={activeTab === tab.id}
          onClick={() => onChange(tab.id)}
          variant={variant}
          disabled={tab.disabled}
        >
          {tab.icon && <span className="tab-icon">{tab.icon}</span>}
          {tab.label}
        </Tab>
      ))}
    </TabsContainer>
  );
};

// Componenti interni
const TabsContainer = styled.div`
  display: flex;
  gap: 2px;
  margin-bottom: 20px;
  border-bottom: ${props => props.variant === 'default' ? '1px solid #ddd' : 'none'};
`;

const Tab = styled.div`
  padding: 10px 20px;
  background: ${props => props.active 
    ? props.variant === 'default' ? 'white' : '#0066cc' 
    : props.variant === 'default' ? '#eee' : 'transparent'
  };
  color: ${props => 
    props.active 
      ? props.variant === 'default' ? '#333' : 'white'
      : props.variant === 'default' ? '#555' : '#0066cc'
  };
  border-radius: ${props => props.variant === 'default' ? '4px 4px 0 0' : '4px'};
  cursor: pointer;
  font-weight: ${props => props.active ? 'bold' : 'normal'};
  transition: all 0.2s ease;
  user-select: none;
  display: flex;
  align-items: center;
  position: relative;
  
  ${props => props.variant === 'default' && props.active && `
    border: 1px solid #ddd;
    border-bottom: 1px solid white;
    margin-bottom: -1px;
  `}
  
  ${props => props.variant === 'pills' && `
    border: 1px solid ${props.active ? '#0066cc' : 'transparent'};
  `}
  
  &:hover {
    background: ${props => 
      props.active 
        ? props.variant === 'default' ? 'white' : '#0066cc' 
        : props.variant === 'default' ? '#e0e0e0' : 'rgba(0, 102, 204, 0.1)'
    };
  }
  
  .tab-icon {
    margin-right: 8px;
    display: flex;
    align-items: center;
  }
  
  ${props => props.disabled && `
    opacity: 0.5;
    cursor: not-allowed;
    pointer-events: none;
  `}
`;

export default Tabs;

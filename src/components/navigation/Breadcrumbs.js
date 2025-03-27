import React from 'react';
import styled from 'styled-components';

const Breadcrumbs = ({ items = [] }) => {
  return (
    <BreadcrumbsContainer>
      {items.map((item, index) => (
        <React.Fragment key={index}>
          {index > 0 && <Separator>/</Separator>}
          <BreadcrumbItem active={index === items.length - 1}>
            {item.url ? <a href={item.url}>{item.label}</a> : item.label}
          </BreadcrumbItem>
        </React.Fragment>
      ))}
    </BreadcrumbsContainer>
  );
};

const BreadcrumbsContainer = styled.div`
  display: flex;
  align-items: center;
  margin-bottom: 15px;
  font-size: 14px;
`;

const BreadcrumbItem = styled.div`
  color: ${props => props.active ? '#333' : '#0066cc'};
  font-weight: ${props => props.active ? 'bold' : 'normal'};
  
  a {
    color: inherit;
    text-decoration: none;
    
    &:hover {
      text-decoration: underline;
    }
  }
`;

const Separator = styled.span`
  margin: 0 8px;
  color: #ccc;
`;

export default Breadcrumbs;